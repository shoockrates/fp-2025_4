{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
module Lib4(runParser, parseCommand) where

import qualified Lib1
import Test.QuickCheck (Arbitrary, Gen, arbitrary, oneof, listOf, elements, choose)

import Control.Applicative (Alternative(..), optional)
import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf)

type ErrorMsg = String
type Input = String
type Parser = ExceptT ErrorMsg (State Input)

-- Basic parsers
keyword :: String -> Parser String
keyword prefix = ExceptT $ do
  input <- get
  if prefix `isPrefixOf` input
    then put (drop (length prefix) input) >> return (Right prefix)
    else return (Left (prefix ++ " expected, got: " ++ take 20 input))

ws :: Parser String
ws = ExceptT $ do
  input <- get
  let spaces = takeWhile isSpace input
      rest = dropWhile isSpace input
  put rest
  return (Right spaces)

parseString :: Parser String
parseString = ExceptT $ do
  input <- get
  case input of
    ('"':rest) ->
      let (str, remaining) = break (== '"') rest
      in case remaining of
           ('"':rest') -> put rest' >> return (Right str)
           _ -> return (Left "Unterminated string")
    _ -> return (Left "Expected quoted string")

parseInt :: Parser Integer
parseInt = ExceptT $ do
  input <- get
  let (digits, rest) = span isDigit input
  if null digits
    then return (Left "Expected integer")
    else put rest >> return (Right (read digits))

parseDouble :: Parser Double
parseDouble = ExceptT $ do
  input <- get
  let (intPart, rest1) = span isDigit input
  if null intPart
    then return (Left "Expected number")
    else case rest1 of
      ('.':rest2) ->
        let (fracPart, rest3) = span isDigit rest2
        in put rest3 >> return (Right (read (intPart ++ "." ++ fracPart)))
      _ -> put rest1 >> return (Right (read intPart))

-- <game_type>
parseGameType :: Parser Lib1.GameType
parseGameType = ExceptT $ do
  input <- get
  case dropWhile isSpace input of
    ('B':'l':'a':'c':'k':'j':'a':'c':'k':rest) -> put rest >> return (Right Lib1.Blackjack)
    ('R':'o':'u':'l':'e':'t':'t':'e':rest) -> put rest >> return (Right Lib1.Roulette)
    ('P':'o':'k':'e':'r':rest) -> put rest >> return (Right Lib1.Poker)
    ('B':'a':'c':'c':'a':'r':'a':'t':rest) -> put rest >> return (Right Lib1.Baccarat)
    ('S':'l':'o':'t':'s':rest) -> put rest >> return (Right Lib1.Slots)
    _ -> return (Left "Unknown game type")

-- <bet_type>
parseBetType :: Parser Lib1.BetType
parseBetType = ExceptT $ do
  input <- get
  case dropWhile isSpace input of
    ('S':'t':'r':'a':'i':'g':'h':'t':rest) -> put rest >> return (Right Lib1.Straight)
    ('S':'p':'l':'i':'t':rest) -> put rest >> return (Right Lib1.Split)
    ('C':'o':'r':'n':'e':'r':rest) -> put rest >> return (Right Lib1.Corner)
    ('R':'e':'d':rest) -> put rest >> return (Right Lib1.Red)
    ('B':'l':'a':'c':'k':rest) -> put rest >> return (Right Lib1.Black)
    ('O':'d':'d':rest) -> put rest >> return (Right Lib1.Odd)
    ('E':'v':'e':'n':rest) -> put rest >> return (Right Lib1.Even)
    ('P':'a':'s':'s':rest) -> put rest >> return (Right Lib1.Pass)
    ('D':'o':'n':'t':'P':'a':'s':'s':rest) -> put rest >> return (Right Lib1.DontPass)
    _ -> return (Left "Unknown bet type")

-- <round_status>
parseRoundStatus :: Parser Lib1.RoundStatus
parseRoundStatus = ExceptT $ do
  input <- get
  case dropWhile isSpace input of
    ('A':'c':'t':'i':'v':'e':rest) -> put rest >> return (Right Lib1.Active)
    ('F':'i':'n':'i':'s':'h':'e':'d':rest) -> put rest >> return (Right Lib1.Finished)
    ('C':'a':'n':'c':'e':'l':'l':'e':'d':rest) -> put rest >> return (Right Lib1.Cancelled)
    _ -> return (Left "Expected round status (Active|Finished|Cancelled)")

-- <bet_outcome>
parseBetOutcome :: Parser Lib1.BetOutcome
parseBetOutcome = ExceptT $ do
  input <- get
  case dropWhile isSpace input of
    ('W':'i':'n':rest) ->
      case runState (runExceptT (ws *> parseDouble)) rest of
        (Right v, rest') -> put rest' >> return (Right (Lib1.Win v))
        _ -> return (Left "Expected number after Win")
    ('L':'o':'s':'e':rest) -> put rest >> return (Right Lib1.Lose)
    ('P':'u':'s':'h':rest) -> put rest >> return (Right Lib1.Push)
    _ -> return (Left "Expected bet outcome (Win|Lose|Push)")

-- <limit_type>
parseLimitType :: Parser Lib1.LimitType
parseLimitType = ExceptT $ do
  input <- get
  case dropWhile isSpace input of
    ('D':'a':'i':'l':'y':'L':'i':'m':'i':'t':rest) -> put rest >> return (Right Lib1.DailyLimit)
    ('W':'e':'e':'k':'l':'y':'L':'i':'m':'i':'t':rest) -> put rest >> return (Right Lib1.WeeklyLimit)
    ('M':'o':'n':'t':'h':'l':'y':'L':'i':'m':'i':'t':rest) -> put rest >> return (Right Lib1.MonthlyLimit)
    _ -> return (Left "Expected limit type")

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

-- Run parser on input
runParser :: Parser a -> String -> Either String a
runParser p input =
  case runState (runExceptT p) input of
    (Left err, _) -> Left err
    (Right result, remaining) ->
      if all isSpace remaining then Right result
      else Left $ "Unconsumed input: " ++ remaining

-- Arbitrary instances for QuickCheck
safeString :: Gen String
safeString = listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")

safeInteger :: Gen Integer
safeInteger = choose (0, 1000)

safeDouble :: Gen Double
safeDouble = choose (0.0, 10000.0)

instance Arbitrary Lib1.GameType where
  arbitrary = elements [Lib1.Blackjack, Lib1.Roulette, Lib1.Poker, Lib1.Baccarat, Lib1.Slots]

instance Arbitrary Lib1.BetType where
  arbitrary = elements [Lib1.Straight, Lib1.Split, Lib1.Corner, Lib1.Red, Lib1.Black, Lib1.Odd, Lib1.Even, Lib1.Pass, Lib1.DontPass]

instance Arbitrary Lib1.RoundStatus where
  arbitrary = elements [Lib1.Active, Lib1.Finished, Lib1.Cancelled]

instance Arbitrary Lib1.BetOutcome where
  arbitrary = oneof
    [ Lib1.Win <$> safeDouble
    , pure Lib1.Lose
    , pure Lib1.Push
    ]

instance Arbitrary Lib1.LimitType where
  arbitrary = elements [Lib1.DailyLimit, Lib1.WeeklyLimit, Lib1.MonthlyLimit]

instance Arbitrary Lib1.Dumpable where
  arbitrary = pure Lib1.Examples

instance Arbitrary Lib1.Command where
  arbitrary = oneof
    [ Lib1.Dump <$> arbitrary
    , Lib1.AddPlayer <$> safeInteger <*> safeString <*> safeDouble
    , Lib1.AddGame <$> safeInteger <*> safeString <*> arbitrary
    , Lib1.AddDealer <$> safeInteger <*> safeString <*> safeInteger
    , Lib1.AddTable <$> safeInteger <*> safeString <*> safeInteger <*> safeDouble <*> safeDouble <*> (oneof [Just <$> safeInteger, pure Nothing])
    , Lib1.AddRound <$> safeInteger <*> safeInteger <*> (oneof [Just <$> safeInteger, pure Nothing]) <*> (oneof [Just <$> arbitrary, pure Nothing])
    , Lib1.PlaceBet <$> safeInteger <*> safeInteger <*> safeInteger <*> safeDouble <*> arbitrary <*> (oneof [Just <$> safeInteger, pure Nothing]) <*> safeInteger
    , Lib1.ResolveBet <$> safeInteger <*> arbitrary
    , Lib1.Deposit <$> safeInteger <*> safeDouble
    , Lib1.Withdraw <$> safeInteger <*> safeDouble
    , Lib1.SetLimit <$> safeInteger <*> arbitrary <*> safeDouble
    , pure Lib1.ShowPlayers
    , pure Lib1.ShowGames
    , pure Lib1.ShowTables
    , pure Lib1.ShowBets
    , pure Lib1.ShowRounds
    , Lib1.RemovePlayer <$> safeInteger
    ]
