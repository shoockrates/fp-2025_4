{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import qualified Lib1
import qualified Lib2
import qualified Lib3
import Lib4 (runParser, parseCommand)

import System.IO.Strict (readFile)
import System.IO (writeFile)
import Control.Exception (catch, IOException, finally)
import Control.Concurrent.MVar
import Control.Concurrent(forkIO, threadDelay)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Control.Monad (foldM, forever)
import Prelude hiding (readFile)
import Control.Concurrent (Chan, newChan, writeChan, readChan)


-- State
type AppState = Lib3.State
stateFile :: FilePath
stateFile = "casino_state.txt"

-- Load state
loadState :: IO AppState
loadState = do
  result <- catch
              (Right <$> readFile stateFile)
              (\(_ :: IOException) -> return (Left "file not found"))

  putStrLn $ "Loaded state:"
  case result of
    Left _ -> return Lib3.emptyState
    Right content -> do
      let linesOfCmds = lines content
      mapM_ putStrLn linesOfCmds
      foldM applyLine Lib3.emptyState linesOfCmds
      

  where
    applyLine :: Lib3.State -> String -> IO Lib3.State
    applyLine st line =
      case runParser parseCommand line of
        Right cmd ->
          let newState = Lib3.applyCommand st cmd
          in return newState
        _ -> return st


-- Save state
saveState :: AppState -> IO ()
saveState st = do
  let commands = Lib3.stateToCommands st
      content = unlines commands
  writeFile stateFile content


-- Running commands
applyCommand :: Lib3.State -> Lib1.Command -> (Lib3.State, String)
applyCommand state cmd = case cmd of
  Lib1.Dump Lib1.Examples ->
    (state, "Examples:\n" ++ unlines (map Lib2.toCliCommand Lib1.examples))

  Lib1.ShowPlayers ->
    let Lib3.State ps _ _ _ _ _ = state
    in (state, "Players:\n" ++ unlines (map show ps))

  Lib1.ShowGames ->
    let Lib3.State _ gs _ _ _ _ = state
    in (state, "Games:\n" ++ unlines (map show gs))

  Lib1.ShowTables ->
    let Lib3.State _ _ _ ts _ _ = state
    in (state, "Tables:\n" ++ unlines (map show ts))

  Lib1.ShowBets ->
    let Lib3.State _ _ _ _ _ bs = state
    in (state, "Bets:\n" ++ unlines (map show bs))

  Lib1.ShowRounds ->
    let Lib3.State _ _ _ _ rs _ = state
    in (state, "Rounds:\n" ++ unlines (map show rs))

  _ ->
    let newState = Lib3.applyCommand state cmd
    in (newState, "Command executed: " ++ Lib2.toCliCommand cmd)

-- Server
main :: IO ()
main = do
  initial <- loadState
  stateVar <- newMVar initial

  _ <- forkIO $ forever $ do
    threadDelay (30 * 1000000)
    currentState <- readMVar stateVar
    saveState currentState

  putStrLn "Server running at http://localhost:3000/cmd"

  let runServer = scotty 3000 $ do
        post "/cmd" $ do
          raw <- body
          let input = TL.unpack (TLE.decodeUtf8 raw)
          case runParser parseCommand input of
            Left err -> text (TL.pack ("Parse error: " ++ err))
            Right cmd -> do
              response <- liftIO $
                modifyMVar stateVar $ \st -> do
                  let (updated, msg) = applyCommand st cmd
                  saveState updated  -- immediate save
                  return (updated, msg)
              text (TL.pack response)
    
  runServer `finally` do
    finalState <- readMVar stateVar
    putStrLn "DEBUG: Saving state on shutdown"
    saveState finalState

