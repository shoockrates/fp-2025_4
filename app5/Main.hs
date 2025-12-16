{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Control.Monad.Free
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import qualified Lib1
import qualified Lib2
import qualified Lib3
import qualified Lib4

import Network.HTTP.Client as HC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as BL

-- Free Monad DSL
data CommandF next where
    DumpF           :: Lib1.Dumpable -> (() -> next) -> CommandF next
    AddPlayerF      :: Integer -> String -> Double -> (() -> next) -> CommandF next
    AddGameF        :: Integer -> String -> Lib1.GameType -> (() -> next) -> CommandF next
    AddDealerF      :: Integer -> String -> Integer -> (() -> next) -> CommandF next
    AddTableF       :: Integer -> String -> Integer -> Double -> Double -> Maybe Integer -> (() -> next) -> CommandF next
    AddRoundF       :: Integer -> Integer -> Maybe Integer -> Maybe Lib1.RoundStatus -> (() -> next) -> CommandF next
    PlaceBetF       :: Integer -> Integer -> Integer -> Double -> Lib1.BetType -> Maybe Integer -> Integer -> (() -> next) -> CommandF next
    ResolveBetF     :: Integer -> Lib1.BetOutcome -> (() -> next) -> CommandF next
    DepositF        :: Integer -> Double -> (() -> next) -> CommandF next
    WithdrawF       :: Integer -> Double -> (() -> next) -> CommandF next
    SetLimitF       :: Integer -> Lib1.LimitType -> Double -> (() -> next) -> CommandF next
    ShowPlayersF    :: (() -> next) -> CommandF next
    ShowGamesF      :: (() -> next) -> CommandF next
    ShowTablesF     :: (() -> next) -> CommandF next
    ShowBetsF       :: (() -> next) -> CommandF next
    ShowRoundsF     :: (() -> next) -> CommandF next
    RemovePlayerF   :: Integer -> (() -> next) -> CommandF next

instance Functor CommandF where
    fmap f (DumpF d next) = DumpF d (f . next)
    fmap f (AddPlayerF pid name bal next) = AddPlayerF pid name bal (f . next)
    fmap f (AddGameF gid name gt next) = AddGameF gid name gt (f . next)
    fmap f (AddDealerF did name tref next) = AddDealerF did name tref (f . next)
    fmap f (AddTableF tid tname gref minb maxb mdr next) = AddTableF tid tname gref minb maxb mdr (f . next)
    fmap f (AddRoundF rid tref prid mstatus next) = AddRoundF rid tref prid mstatus (f . next)
    fmap f (PlaceBetF bid pref tref amt btype pbid rref next) = PlaceBetF bid pref tref amt btype pbid rref (f . next)
    fmap f (ResolveBetF bre outcome next) = ResolveBetF bre outcome (f . next)
    fmap f (DepositF pref amt next) = DepositF pref amt (f . next)
    fmap f (WithdrawF pref amt next) = WithdrawF pref amt (f . next)
    fmap f (SetLimitF pref ltype amt next) = SetLimitF pref ltype amt (f . next)
    fmap f (ShowPlayersF next) = ShowPlayersF (f . next)
    fmap f (ShowGamesF next) = ShowGamesF (f . next)
    fmap f (ShowTablesF next) = ShowTablesF (f . next)
    fmap f (ShowBetsF next) = ShowBetsF (f . next)
    fmap f (ShowRoundsF next) = ShowRoundsF (f . next)
    fmap f (RemovePlayerF pid next) = RemovePlayerF pid (f . next)

type DSL = Free CommandF

-- DSL functions
dump :: Lib1.Dumpable -> DSL ()
dump d = liftF (DumpF d id)

addPlayer :: Integer -> String -> Double -> DSL ()
addPlayer pid name bal = liftF (AddPlayerF pid name bal id)

addGame :: Integer -> String -> Lib1.GameType -> DSL ()
addGame gid name gt = liftF (AddGameF gid name gt id)

addDealer :: Integer -> String -> Integer -> DSL ()
addDealer did name tref = liftF (AddDealerF did name tref id)

addTable :: Integer -> String -> Integer -> Double -> Double -> Maybe Integer -> DSL ()
addTable tid tname gref minb maxb mdr = liftF (AddTableF tid tname gref minb maxb mdr id)

addRound :: Integer -> Integer -> Maybe Integer -> Maybe Lib1.RoundStatus -> DSL ()
addRound rid tref prid mstatus = liftF (AddRoundF rid tref prid mstatus id)

placeBet :: Integer -> Integer -> Integer -> Double -> Lib1.BetType -> Maybe Integer -> Integer -> DSL ()
placeBet bid pref tref amt btype pbid rref = liftF (PlaceBetF bid pref tref amt btype pbid rref id)

resolveBet :: Integer -> Lib1.BetOutcome -> DSL ()
resolveBet bre outcome = liftF (ResolveBetF bre outcome id)

deposit :: Integer -> Double -> DSL ()
deposit pref amt = liftF (DepositF pref amt id)

withdraw :: Integer -> Double -> DSL ()
withdraw pref amt = liftF (WithdrawF pref amt id)

setLimit :: Integer -> Lib1.LimitType -> Double -> DSL ()
setLimit pref ltype amt = liftF (SetLimitF pref ltype amt id)

showPlayers :: DSL ()
showPlayers = liftF (ShowPlayersF id)

showGames :: DSL ()
showGames = liftF (ShowGamesF id)

showTables :: DSL ()
showTables = liftF (ShowTablesF id)

showBets :: DSL ()
showBets = liftF (ShowBetsF id)

showRounds :: DSL ()
showRounds = liftF (ShowRoundsF id)

removePlayer :: Integer -> DSL ()
removePlayer pid = liftF (RemovePlayerF pid id)

-- Server interpreter
interpretServer :: DSL a -> IO a
interpretServer (Pure a) = return a
interpretServer (Free cmd) = case cmd of
    DumpF Lib1.Examples next -> send (Lib2.toCliCommand (Lib1.Dump Lib1.Examples)) >> interpretServer (next ())
    AddPlayerF pid name bal next -> send (Lib2.toCliCommand (Lib1.AddPlayer pid name bal)) >> interpretServer (next ())
    AddGameF gid name gt next -> send (Lib2.toCliCommand (Lib1.AddGame gid name gt)) >> interpretServer (next ())
    AddDealerF did name tref next -> send (Lib2.toCliCommand (Lib1.AddDealer did name tref)) >> interpretServer (next ())
    AddTableF tid tname gref minb maxb mdr next -> send (Lib2.toCliCommand (Lib1.AddTable tid tname gref minb maxb mdr)) >> interpretServer (next ())
    AddRoundF rid tref prid mstatus next -> send (Lib2.toCliCommand (Lib1.AddRound rid tref prid mstatus)) >> interpretServer (next ())
    PlaceBetF bid pref tref amt btype pbid rref next -> send (Lib2.toCliCommand (Lib1.PlaceBet bid pref tref amt btype pbid rref)) >> interpretServer (next ())
    ResolveBetF bre outcome next -> send (Lib2.toCliCommand (Lib1.ResolveBet bre outcome)) >> interpretServer (next ())
    DepositF pref amt next -> send (Lib2.toCliCommand (Lib1.Deposit pref amt)) >> interpretServer (next ())
    WithdrawF pref amt next -> send (Lib2.toCliCommand (Lib1.Withdraw pref amt)) >> interpretServer (next ())
    SetLimitF pref ltype amt next -> send (Lib2.toCliCommand (Lib1.SetLimit pref ltype amt)) >> interpretServer (next ())
    ShowPlayersF next -> send (Lib2.toCliCommand Lib1.ShowPlayers) >> interpretServer (next ())
    ShowGamesF next -> send (Lib2.toCliCommand Lib1.ShowGames) >> interpretServer (next ())
    ShowTablesF next -> send (Lib2.toCliCommand Lib1.ShowTables) >> interpretServer (next ())
    ShowBetsF next -> send (Lib2.toCliCommand Lib1.ShowBets) >> interpretServer (next ())
    ShowRoundsF next -> send (Lib2.toCliCommand Lib1.ShowRounds) >> interpretServer (next ())
    RemovePlayerF pid next -> send (Lib2.toCliCommand (Lib1.RemovePlayer pid)) >> interpretServer (next ())

-- HTTP send function
send :: String -> IO ()
send cmdStr = do
    manager <- HC.newManager tlsManagerSettings
    req0 <- HC.parseRequest "POST http://localhost:3000/cmd"
    let req = req0 { HC.method = "POST"
                   , HC.requestBody = HC.RequestBodyLBS (BL.pack cmdStr)
                   }
    resp <- HC.httpLbs req manager
    putStrLn $ "Server response: " ++ BL.unpack (HC.responseBody resp)

-- Local interpreter
interpretLocal :: DSL a -> StateT Lib3.State IO a
interpretLocal (Pure a) = return a
interpretLocal (Free cmd) = case cmd of
    DumpF Lib1.Examples next -> 
        liftIO (putStrLn "Examples:" >> mapM_ (putStrLn . Lib2.toCliCommand) Lib1.examples) >> interpretLocal (next ())

    AddPlayerF pid name bal next -> do
        modify (\st -> Lib3.applyCommand st (Lib1.AddPlayer pid name bal))
        liftIO $ putStrLn $ "Added player: " ++ show pid ++ " " ++ name ++ " " ++ show bal
        interpretLocal (next ())

    AddGameF gid name gt next -> do
        modify (\st -> Lib3.applyCommand st (Lib1.AddGame gid name gt))
        liftIO $ putStrLn $ "Added game: " ++ show gid ++ " " ++ name ++ " " ++ show gt
        interpretLocal (next ())

    AddDealerF did name tref next -> do
        modify (\st -> Lib3.applyCommand st (Lib1.AddDealer did name tref))
        liftIO $ putStrLn $ "Added dealer: " ++ show did ++ " " ++ name ++ " " ++ show tref
        interpretLocal (next ())

    AddTableF tid tname gref minb maxb mdr next -> do
        modify (\st -> Lib3.applyCommand st (Lib1.AddTable tid tname gref minb maxb mdr))
        liftIO $ putStrLn $ "Added table: " ++ show tid ++ " " ++ tname
        interpretLocal (next ())

    AddRoundF rid tref prid mstatus next -> do
        modify (\st -> Lib3.applyCommand st (Lib1.AddRound rid tref prid mstatus))
        liftIO $ putStrLn $ "Added round: " ++ show rid
        interpretLocal (next ())

    PlaceBetF bid pref tref amt btype pbid rref next -> do
        modify (\st -> Lib3.applyCommand st (Lib1.PlaceBet bid pref tref amt btype pbid rref))
        liftIO $ putStrLn $ "Placed bet: " ++ show bid
        interpretLocal (next ())

    ResolveBetF bre outcome next -> do
        modify (\st -> Lib3.applyCommand st (Lib1.ResolveBet bre outcome))
        liftIO $ putStrLn $ "Resolved bet: " ++ show bre ++ " " ++ show outcome
        interpretLocal (next ())

    DepositF pref amt next -> do
        modify (\st -> Lib3.applyCommand st (Lib1.Deposit pref amt))
        liftIO $ putStrLn $ "Deposit: " ++ show pref ++ " " ++ show amt
        interpretLocal (next ())

    WithdrawF pref amt next -> do
        modify (\st -> Lib3.applyCommand st (Lib1.Withdraw pref amt))
        liftIO $ putStrLn $ "Withdraw: " ++ show pref ++ " " ++ show amt
        interpretLocal (next ())

    SetLimitF pref ltype amt next -> do
        modify (\st -> Lib3.applyCommand st (Lib1.SetLimit pref ltype amt))
        liftIO $ putStrLn $ "Set limit: " ++ show pref ++ " " ++ show ltype ++ " " ++ show amt
        interpretLocal (next ())

    ShowPlayersF next -> do
        Lib3.State ps _ _ _ _ _ <- get
        liftIO $ putStrLn "Players:" >> mapM_ print ps
        interpretLocal (next ())

    ShowGamesF next -> do
        Lib3.State _ gs _ _ _ _ <- get
        liftIO $ putStrLn "Games:" >> mapM_ print gs
        interpretLocal (next ())

    ShowTablesF next -> do
        Lib3.State _ _ _ ts _ _ <- get
        liftIO $ putStrLn "Tables:" >> mapM_ print ts
        interpretLocal (next ())

    ShowBetsF next -> do
        Lib3.State _ _ _ _ _ bs <- get
        liftIO $ putStrLn "Bets:" >> mapM_ print bs
        interpretLocal (next ())

    ShowRoundsF next -> do
        Lib3.State _ _ _ _ rs _ <- get
        liftIO $ putStrLn "Rounds:" >> mapM_ print rs
        interpretLocal (next ())

    RemovePlayerF pid next -> do
        modify (\st -> Lib3.applyCommand st (Lib1.RemovePlayer pid))
        liftIO $ putStrLn $ "Removed player: " ++ show pid
        interpretLocal (next ())

-- Example usage
exampleDSL :: DSL ()
exampleDSL = do 
    addPlayer 1 "John Smith" 1000.0
    addGame 1 "European Roulette" Lib1.Roulette
    addDealer 1 "Maria Garcia" 1
    addTable 1 "High Roller Roulette" 1 100.0 5000.0 (Just 1)
    addRound 1 1 Nothing (Just Lib1.Active)
    placeBet 1 1 1 500.0 Lib1.Red Nothing 1
    deposit 1 2000.0
    showPlayers
    showGames

-- Main
main :: IO ()
main = do
    putStrLn "Running DSL locally:"
    _ <- evalStateT (interpretLocal exampleDSL) Lib3.emptyState

    putStrLn "\nRunning DSL against server:"
    _ <- interpretServer exampleDSL
    return ()

