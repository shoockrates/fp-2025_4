{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}
module Lib3(
    emptyState, State(..), execute, load, save, storageOpLoop, StorageOp, Parser(..), parseCommand) where

import qualified Lib1
import qualified Lib2

import Data.List (isPrefixOf, find, intercalate)
import Data.Char (isDigit, isSpace)
import Data.Either (isRight)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar(TVar, readTVar, writeTVar)
import Control.Concurrent (Chan, readChan, writeChan, newChan)
import System.IO (writeFile)
import System.IO.Strict (readFile)
import qualified System.IO.Strict as Strict
import Control.Applicative
import Control.Monad (forever)
import Control.Exception

newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Left e -> Left e
      Right (v, rest) -> Right (f v, rest)

instance Applicative Parser where
  pure a = Parser $ \input -> Right (a, input)
  (Parser pf) <*> (Parser pa) = Parser $ \input ->
    case pf input of
      Left e1 -> Left e1
      Right (f, rest1) ->
        case pa rest1 of
          Left e2 -> Left e2
          Right (a, rest2) -> Right (f a, rest2)

instance Alternative Parser where
  empty = Parser $ \_ -> Left "No alternatives"
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    case p1 input of
      Right r -> Right r
      Left _ -> p2 input

-- Basic parsers
keyword :: String -> Parser String
keyword prefix = Parser $ \input ->
  if prefix `isPrefixOf` input
  then Right (prefix, drop (length prefix) input)
  else Left (prefix ++ " expected, got: " ++ take 20 input)

ws :: Parser String
ws = Parser $ \input ->
  let spaces = takeWhile isSpace input
      rest = dropWhile isSpace input
  in Right (spaces, rest)

parseString :: Parser String
parseString = Parser $ \input ->
  case input of
    ('"':rest) ->
      let (str, remaining) = break (== '"') rest
      in case remaining of
           ('"':rest') -> Right (str, rest')
           _ -> Left "Unterminated string"
    _ -> Left "Expected quoted string"

parseInt :: Parser Integer
parseInt = Parser $ \input ->
  let (digits, rest) = span isDigit input
  in if null digits
     then Left "Expected integer"
     else Right (read digits, rest)

parseDouble :: Parser Double
parseDouble = Parser $ \input ->
  let (intPart, rest1) = span isDigit input
  in if null intPart
     then Left "Expected number"
     else case rest1 of
       ('.':rest2) ->
         let (fracPart, rest3) = span isDigit rest2
         in Right (read (intPart ++ "." ++ fracPart), rest3)
       _ -> Right (read intPart, rest1)

-- <game_type>
parseGameType :: Parser Lib1.GameType
parseGameType = Parser $ \input ->
  case dropWhile isSpace input of
    ('B':'l':'a':'c':'k':'j':'a':'c':'k':rest) -> Right (Lib1.Blackjack, rest)
    ('R':'o':'u':'l':'e':'t':'t':'e':rest) -> Right (Lib1.Roulette, rest)
    ('P':'o':'k':'e':'r':rest) -> Right (Lib1.Poker, rest)
    ('B':'a':'c':'c':'a':'r':'a':'t':rest) -> Right (Lib1.Baccarat, rest)
    ('S':'l':'o':'t':'s':rest) -> Right (Lib1.Slots, rest)
    _ -> Left "Unknown game type"

-- <bet_type>
parseBetType :: Parser Lib1.BetType
parseBetType = Parser $ \input ->
  case dropWhile isSpace input of
    ('S':'t':'r':'a':'i':'g':'h':'t':rest) -> Right (Lib1.Straight, rest)
    ('S':'p':'l':'i':'t':rest) -> Right (Lib1.Split, rest)
    ('C':'o':'r':'n':'e':'r':rest) -> Right (Lib1.Corner, rest)
    ('R':'e':'d':rest) -> Right (Lib1.Red, rest)
    ('B':'l':'a':'c':'k':rest) -> Right (Lib1.Black, rest)
    ('O':'d':'d':rest) -> Right (Lib1.Odd, rest)
    ('E':'v':'e':'n':rest) -> Right (Lib1.Even, rest)
    ('P':'a':'s':'s':rest) -> Right (Lib1.Pass, rest)
    ('D':'o':'n':'t':'P':'a':'s':'s':rest) -> Right (Lib1.DontPass, rest)
    _ -> Left "Unknown bet type"

-- <round_status>
parseRoundStatus :: Parser Lib1.RoundStatus
parseRoundStatus = Parser $ \input ->
  case dropWhile isSpace input of
    ('A':'c':'t':'i':'v':'e':rest) -> Right (Lib1.Active, rest)
    ('F':'i':'n':'i':'s':'h':'e':'d':rest) -> Right (Lib1.Finished, rest)
    ('C':'a':'n':'c':'e':'l':'l':'e':'d':rest) -> Right (Lib1.Cancelled, rest)
    _ -> Left "Expected round status (Active|Finished|Cancelled)"

-- <bet_outcome>
parseBetOutcome :: Parser Lib1.BetOutcome
parseBetOutcome = Parser $ \input ->
  case dropWhile isSpace input of
    ('W':'i':'n':rest) ->
      case runParser (ws *> parseDouble) rest of
        Right (v, rest') -> Right (Lib1.Win v, rest')
        Left _ -> Left "Expected number after Win"
    ('L':'o':'s':'e':rest) -> Right (Lib1.Lose, rest)
    ('P':'u':'s':'h':rest) -> Right (Lib1.Push, rest)
    _ -> Left "Expected bet outcome (Win|Lose|Push)"

-- <limit_type>
parseLimitType :: Parser Lib1.LimitType
parseLimitType = Parser $ \input ->
  case dropWhile isSpace input of
    ('D':'a':'i':'l':'y':'L':'i':'m':'i':'t':rest) -> Right (Lib1.DailyLimit, rest)
    ('W':'e':'e':'k':'l':'y':'L':'i':'m':'i':'t':rest) -> Right (Lib1.WeeklyLimit, rest)
    ('M':'o':'n':'t':'h':'l':'y':'L':'i':'m':'i':'t':rest) -> Right (Lib1.MonthlyLimit, rest)
    _ -> Left "Expected limit type"

-- Command parsers
-- <dump> ::= "Dump" <ws> "Examples"
parseDump :: Parser Lib1.Command
parseDump =
  Lib1.Dump Lib1.Examples <$ keyword "Dump" <* ws <* keyword "Examples"

-- <add_player> ::= "AddPlayer" <ws> <integer> <ws> <string> <ws> <double>
parseAddPlayer :: Parser Lib1.Command
parseAddPlayer =
  Lib1.AddPlayer <$> (keyword "AddPlayer" *> ws *> parseInt)
                 <*> (ws *> parseString)
                 <*> (ws *> parseDouble)

-- <add_game> ::= "AddGame" <ws> <integer> <ws> <string> <ws> <game_type>
parseAddGame :: Parser Lib1.Command
parseAddGame =
  Lib1.AddGame <$> (keyword "AddGame" *> ws *> parseInt)
               <*> (ws *> parseString)
               <*> (ws *> parseGameType)

-- <add_dealer> ::= "AddDealer" <ws> <integer> <ws> <string> <ws> <integer>
parseAddDealer :: Parser Lib1.Command
parseAddDealer =
  Lib1.AddDealer <$> (keyword "AddDealer" *> ws *> parseInt)
                  <*> (ws *> parseString)
                  <*> (ws *> parseInt)

-- <add_table> ::= "AddTable" <ws> <integer> <ws> <string> <ws> <integer> <ws> <double> <ws> <double> [<ws> <integer>]
parseAddTable :: Parser Lib1.Command
parseAddTable =
  Lib1.AddTable <$> (keyword "AddTable" *> ws *> parseInt)
                <*> (ws *> parseString)
                <*> (ws *> parseInt)
                <*> (ws *> parseDouble)
                <*> (ws *> parseDouble)
                <*> (optional (ws *> parseInt))

-- <add_round> ::= "AddRound" <ws> <integer> <ws> <integer> [<ws> <integer>] [<ws> <round_status>]
parseAddRound :: Parser Lib1.Command
parseAddRound =
  Lib1.AddRound <$> (keyword "AddRound" *> ws *> parseInt)
                <*> (ws *> parseInt)
                <*> optional (ws *> parseInt)
                <*> optional (ws *> parseRoundStatus)

-- <place_bet> ::= "PlaceBet" <ws> <integer> <ws> <integer> <ws> <integer> <ws> <double> <ws> <bet_type> [<ws> "parent" <ws> <integer>] <ws> "round" <ws> <integer>
parsePlaceBet :: Parser Lib1.Command
parsePlaceBet =
  Lib1.PlaceBet <$> (keyword "PlaceBet" *> ws *> parseInt)
                 <*> (ws *> parseInt)
                 <*> (ws *> parseInt)
                 <*> (ws *> parseDouble)
                 <*> (ws *> parseBetType)
                 <*> optional (ws *> keyword "parent" *> ws *> parseInt)
                 <*> (ws *> keyword "round" *> ws *> parseInt)

-- <resolve_bet> ::= "ResolveBet" <ws> <integer> <ws> <bet_outcome>
parseResolveBet :: Parser Lib1.Command
parseResolveBet =
  Lib1.ResolveBet <$> (keyword "ResolveBet" *> ws *> parseInt)
                  <*> (ws *> parseBetOutcome)

-- <deposit> ::= "Deposit" <ws> <integer> <ws> <double>
parseDeposit :: Parser Lib1.Command
parseDeposit =
  Lib1.Deposit <$> (keyword "Deposit" *> ws *> parseInt)
               <*> (ws *> parseDouble)

-- <withdraw> ::= "Withdraw" <ws> <integer> <ws> <double>
parseWithdraw :: Parser Lib1.Command
parseWithdraw =
  Lib1.Withdraw <$> (keyword "Withdraw" *> ws *> parseInt)
                <*> (ws *> parseDouble)

-- <set_limit> ::= "SetLimit" <ws> <integer> <ws> <limit_type> <ws> <double>
parseSetLimit :: Parser Lib1.Command
parseSetLimit =
  Lib1.SetLimit <$> (keyword "SetLimit" *> ws *> parseInt)
                <*> (ws *> parseLimitType)
                <*> (ws *> parseDouble)

-- <show> ::= "Show" <ws> ("Players" | "Games" | "Tables" | "Bets" | "Rounds")
parseShow :: Parser Lib1.Command
parseShow =
  keyword "Show" *> ws *> (
    Lib1.ShowPlayers <$ keyword "Players" <|>
    Lib1.ShowGames <$ keyword "Games" <|>
    Lib1.ShowTables <$ keyword "Tables" <|>
    Lib1.ShowBets <$ keyword "Bets" <|>
    Lib1.ShowRounds <$ keyword "Rounds"
  )

-- <remove_player> ::= "RemovePlayer" <ws> <integer>
parseRemovePlayer :: Parser Lib1.Command
parseRemovePlayer =
  Lib1.RemovePlayer <$> (keyword "RemovePlayer" *> ws *> parseInt)

-- <command> ::= <dump> | <add_player> | <add_game> | <add_dealer> | <add_table> | <add_round> | <place_bet> | <resolve_bet> | <deposit> | <withdraw> | <set_limit> | <show> | <remove_player>
parseCommand :: Parser Lib1.Command
parseCommand =
  parseDump <|>
  parseAddPlayer <|>
  parseAddGame <|>
  parseAddDealer <|>
  parseAddTable <|>
  parseAddRound <|>
  parsePlaceBet <|>
  parseResolveBet <|>
  parseDeposit <|>
  parseWithdraw <|>
  parseSetLimit <|>
  parseShow <|>
  parseRemovePlayer

-- State definition
data Player = Player
  { playerId :: Integer
  , playerName :: String
  , balance :: Double
  , dailyLimit :: Maybe Double
  , weeklyLimit :: Maybe Double
  , monthlyLimit :: Maybe Double
  } deriving (Show, Eq)

data Game = Game
  { gameId :: Integer
  , gameName :: String
  , gameType :: Lib1.GameType
  } deriving (Show, Eq)

data Dealer = Dealer
  { dealerId :: Integer
  , dealerName :: String
  , tableRef :: Integer
  } deriving (Show, Eq)

data Table = Table
  { tableId :: Integer
  , tableName :: String
  , gameRef :: Integer
  , minBet :: Double
  , maxBet :: Double
  , dealerRef :: Maybe Integer
  } deriving (Show, Eq)

data Round = Round
  { roundId :: Integer
  , roundTableRef :: Integer
  , parentRoundId :: Maybe Integer
  , status :: Maybe Lib1.RoundStatus
  } deriving (Show, Eq)

data Bet = Bet
  { betId :: Integer
  , playerRef :: Integer
  , betTableRef :: Integer
  , amount :: Double
  , betType :: Lib1.BetType
  , parentBetId :: Maybe Integer
  , roundRef :: Integer
  , outcome :: Maybe Lib1.BetOutcome
  } deriving (Show, Eq)

data State = State
  { players :: [Player]
  , games :: [Game]
  , dealers :: [Dealer]
  , tables :: [Table]
  , rounds :: [Round]
  , bets :: [Bet]
  } deriving (Show, Eq)

emptyState :: State
emptyState = State [] [] [] [] [] []

-- Pure domain logic functions
applyCommand :: State -> Lib1.Command -> State
applyCommand state cmd = case cmd of
  Lib1.AddPlayer pid name bal ->
    let newPlayer = Player pid name bal Nothing Nothing Nothing
    in state { players = players state ++ [newPlayer] }
  
  Lib1.AddGame gid name gt ->
    let newGame = Game gid name gt
    in state { games = games state ++ [newGame] }
  
  Lib1.AddDealer did name tref ->
    let newDealer = Dealer did name tref
    in state { dealers = dealers state ++ [newDealer] }
  
  Lib1.AddTable tid tname gref minb maxb mdr ->
    let newTable = Table tid tname gref minb maxb mdr
    in state { tables = tables state ++ [newTable] }
  
  Lib1.AddRound rid tref prid mstatus ->
    let newRound = Round rid tref prid mstatus
    in state { rounds = rounds state ++ [newRound] }
  
  Lib1.PlaceBet bid pref tref amt btype pbid rref ->
    let newBet = Bet bid pref tref amt btype pbid rref Nothing
    in state { bets = bets state ++ [newBet] }
  
  Lib1.ResolveBet bre outcome ->
    let updateBet b = if betId b == bre then b { outcome = Just outcome } else b
    in state { bets = map updateBet (bets state) }
  
  Lib1.Deposit pref amt ->
    let updatePlayer p = if playerId p == pref
                         then p { balance = balance p + amt }
                         else p
    in state { players = map updatePlayer (players state) }
  
  Lib1.Withdraw pref amt ->
    let updatePlayer p = if playerId p == pref
                         then p { balance = balance p - amt }
                         else p
    in state { players = map updatePlayer (players state) }
  
  Lib1.SetLimit pref ltype amt ->
    let updatePlayer p = if playerId p == pref
                         then case ltype of
                           Lib1.DailyLimit -> p { dailyLimit = Just amt }
                           Lib1.WeeklyLimit -> p { weeklyLimit = Just amt }
                           Lib1.MonthlyLimit -> p { monthlyLimit = Just amt }
                         else p
    in state { players = map updatePlayer (players state) }
  
  Lib1.RemovePlayer pid ->
    state { players = filter ((/= pid) . playerId) (players state)
          , bets = filter ((/= pid) . playerRef) (bets state) }
  
  _ -> state  -- Show commands don't modify state

-- Convert state to commands efficiently
stateToCommands :: State -> [String]
stateToCommands state =
  concat
    [ map playerToCommand (players state)
    , map gameToCommand (games state)
    , map dealerToCommand (dealers state)
    , map tableToCommand (tables state)
    , map roundToCommand (rounds state)
    , map betToCommand (bets state)
    , concatMap resolveBetToCommand (bets state)
    , concatMap playerLimitsToCommands (players state)
    ]
  where
    playerToCommand (Player pid name bal _ _ _) =
      Lib2.toCliCommand (Lib1.AddPlayer pid name bal)
    
    gameToCommand (Game gid name gt) =
      Lib2.toCliCommand (Lib1.AddGame gid name gt)
    
    dealerToCommand (Dealer did name tref) =
      Lib2.toCliCommand (Lib1.AddDealer did name tref)
    
    tableToCommand (Table tid tname gref minb maxb mdr) =
      Lib2.toCliCommand (Lib1.AddTable tid tname gref minb maxb mdr)
    
    roundToCommand (Round rid tref prid mstatus) =
      Lib2.toCliCommand (Lib1.AddRound rid tref prid mstatus)
    
    betToCommand (Bet bid pref tref amt btype pbid rref _) =
      Lib2.toCliCommand (Lib1.PlaceBet bid pref tref amt btype pbid rref)
    
    resolveBetToCommand (Bet bid _ _ _ _ _ _ (Just outcome)) =
      [Lib2.toCliCommand (Lib1.ResolveBet bid outcome)]
    resolveBetToCommand _ = []
    
    playerLimitsToCommands (Player pid _ _ dlimit wlimit mlimit) =
      concat
        [ maybe [] (\amt -> [Lib2.toCliCommand (Lib1.SetLimit pid Lib1.DailyLimit amt)]) dlimit
        , maybe [] (\amt -> [Lib2.toCliCommand (Lib1.SetLimit pid Lib1.WeeklyLimit amt)]) wlimit
        , maybe [] (\amt -> [Lib2.toCliCommand (Lib1.SetLimit pid Lib1.MonthlyLimit amt)]) mlimit
        ]

execute :: TVar State -> Lib1.Command -> IO ()
execute tvarState cmd = case cmd of
  Lib1.Dump Lib1.Examples -> do
    putStrLn "Examples:"
    mapM_ (putStrLn . Lib2.toCliCommand) Lib1.examples

  Lib1.ShowPlayers -> do
    State ps _ _ _ _ _ <- readTVarIO tvarState
    putStrLn "Players:"
    mapM_ print ps

  Lib1.ShowGames -> do
    State _ gs _ _ _ _ <- readTVarIO tvarState
    putStrLn "Games:"
    mapM_ print gs

  Lib1.ShowTables -> do
    State _ _ _ ts _ _ <- readTVarIO tvarState
    putStrLn "Tables:"
    mapM_ print ts

  _ -> do
    atomically $ do
      currentState <- readTVar tvarState
      let newState = applyCommand currentState cmd
      writeTVar tvarState newState
    putStrLn $ "Command executed: " ++ Lib2.toCliCommand cmd


-- Storage operations
data StorageOp = Save String (Chan ()) | Load (Chan String)

storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop c = forever $ do
  op <- readChan c
  case op of
    Save content replyChan -> do
      result <- try (writeFile "casino_state.txt" content) :: IO (Either IOException ())
      case result of
        Right () -> return ()
        Left e -> putStrLn $ "Warning: failed to write file, skipping save: " ++ show e
      writeChan replyChan ()
    Load replyChan -> do
      result <- try (Strict.readFile "casino_state.txt") :: IO (Either IOException String)
      case result of
        Right content -> writeChan replyChan content
        Left _ -> writeChan replyChan ""

save :: Chan StorageOp -> TVar State -> IO (Either String ())
save storageChan tvarState = do
  replyChan <- newChan
  state <- readTVarIO tvarState
  let commands = stateToCommands state
      content = unlines commands
  writeChan storageChan (Save content replyChan)
  _ <- readChan replyChan
  return (Right ())

load :: Chan StorageOp -> TVar State -> IO (Either String ())
load storageChan tvarState = do
  replyChan <- newChan
  writeChan storageChan (Load replyChan)
  content <- readChan replyChan
  let cmds = lines content
  
  atomically $ writeTVar tvarState emptyState
  
  result <- mapM (applyLine tvarState) cmds
  if all isRight result
    then return (Right ())
    else return (Left "Error loading state")
  where
    applyLine tv line =
      if null line || all isSpace line
      then return $ Right ()
      else case runParser parseCommand line of
        Left e ->
          return $ Left ("Parse error: " ++ e ++ " in line: " ++ line)
        Right (cmd, rest) ->
          if all isSpace rest
          then do
            execute tv cmd
            return $ Right ()
          else
            return $ Left ("Unparsed trailing text: " ++ rest)
