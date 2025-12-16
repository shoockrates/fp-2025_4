{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use span" #-}
module Lib2(
    parseCommand
  , ToCliCommand(..)
  , process
  ) where

import qualified Lib1
import Data.List (isPrefixOf)
import Data.Char (isDigit, isSpace)

type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)

char :: Parser Char
char (c:cs) = Right (c, cs)
char [] = Left "No character available"

digit :: Parser Char
digit input@(c:cs)
  | isDigit c = Right (c, cs)
  | otherwise = Left ("Expected digit, got: " ++ [c])
digit [] = Left "Expected digit, got empty input"

number :: Parser String
number input =
  let (digits, rest) = span isDigit input
  in if null digits 
     then Left "Expected number" 
     else Right (digits, rest)

string :: Parser String
string ('"':input) =
  let (str, rest) = break (== '"') input
  in case rest of
       ('"':rest') -> Right (str, rest')
       _ -> Left "Unterminated string"
string _ = Left "Expected quoted string"

whitespace :: Parser String
whitespace input = Right (takeWhile isSpace input, dropWhile isSpace input)

-- Repetition combinators
many :: Parser a -> Parser [a]
many p input = case p input of
  Right (v, rest) -> case many p rest of
    Right (vs, rest') -> Right (v:vs, rest')
    Left _ -> Right ([v], rest)
  Left _ -> Right ([], input)

many1 :: Parser a -> Parser [a]
many1 p input = case p input of
  Right (v, rest) -> case many p rest of
    Right (vs, rest') -> Right (v:vs, rest')
    Left _ -> Right ([v], rest)
  Left e -> Left e

and2 :: Parser a -> Parser b -> Parser (a, b)
and2 p1 p2 input =
  case p1 input of
    Left err -> Left err
    Right (v1, rest1) ->
      case p2 rest1 of
        Left err -> Left err
        Right (v2, rest2) -> Right ((v1, v2), rest2)

and3 :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
and3 p1 p2 p3 input =
  case p1 input of
    Left err -> Left err
    Right (v1, rest1) ->
      case p2 rest1 of
        Left err -> Left err
        Right (v2, rest2) ->
          case p3 rest2 of
            Left err -> Left err
            Right (v3, rest3) ->
              Right ((v1, v2, v3), rest3)

and4 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser (a, b, c, d)
and4 p1 p2 p3 p4 input =
  case p1 input of
    Left err -> Left err
    Right (v1, rest1) ->
      case p2 rest1 of
        Left err -> Left err
        Right (v2, rest2) ->
          case p3 rest2 of
            Left err -> Left err
            Right (v3, rest3) ->
              case p4 rest3 of
                Left err -> Left err
                Right (v4, rest4) ->
                  Right ((v1, v2, v3, v4), rest4)

and5 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser (a, b, c, d, e)
and5 p1 p2 p3 p4 p5 input =
  case p1 input of
    Left err -> Left err
    Right (v1, rest1) ->
      case p2 rest1 of
        Left err -> Left err
        Right (v2, rest2) ->
          case p3 rest2 of
            Left err -> Left err
            Right (v3, rest3) ->
              case p4 rest3 of
                Left err -> Left err
                Right (v4, rest4) ->
                  case p5 rest4 of
                    Left err -> Left err
                    Right (v5, rest5) ->
                      Right ((v1, v2, v3, v4, v5), rest5)

and6 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser (a, b, c, d, e, f)
and6 p1 p2 p3 p4 p5 p6 input =
  case p1 input of
    Left err -> Left err
    Right (v1, rest1) ->
      case p2 rest1 of
        Left err -> Left err
        Right (v2, rest2) ->
          case p3 rest2 of
            Left err -> Left err
            Right (v3, rest3) ->
              case p4 rest3 of
                Left err -> Left err
                Right (v4, rest4) ->
                  case p5 rest4 of
                    Left err -> Left err
                    Right (v5, rest5) ->
                      case p6 rest5 of
                        Left err -> Left err
                        Right (v6, rest6) ->
                          Right ((v1, v2, v3, v4, v5, v6), rest6)

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 input = 
  case p1 input of
    Right r -> Right r
    Left _  -> p2 input


pmap :: (a -> b) -> Parser a -> Parser b
pmap f p input =
  case p input of
    Left err -> Left err
    Right (v, rest) -> Right (f v, rest)


specificChar :: Char -> Parser Char
specificChar expected input = case char input of
  Right (c, rest) | c == expected -> Right (c, rest)
                  | otherwise -> Left ("Expected '" ++ [expected] ++ "', got '" ++ [c] ++ "'")
  Left e -> Left e

keyword :: String -> Parser String
keyword prefix input =
  if prefix `isPrefixOf` input
  then Right (prefix, drop (length prefix) input)
  else Left (prefix ++ " expected, got: " ++ input)

ws :: Parser String
ws input = Right (takeWhile (== ' ') input, dropWhile (== ' ') input)

parseString :: Parser String
parseString = string

-- parseInt using number parser
parseInt :: Parser Integer
parseInt input = case number input of
  Right (numStr, rest) -> Right (read numStr, rest)
  Left e -> Left e

-- parseDouble using number and digit parsers
parseDouble :: Parser Double
parseDouble input = case number input of
  Right (intPart, rest) -> case rest of
    ('.':rest') -> case number rest' of
      Right (fracPart, rest'') -> Right (read (intPart ++ "." ++ fracPart), rest'')
      Left _ -> Right (read intPart, rest)
    _ -> Right (read intPart, rest)
  Left e -> Left e

parseRoundStatus :: Parser Lib1.RoundStatus
parseRoundStatus input =
  case dropWhile (== ' ') input of
    ('A':'c':'t':'i':'v':'e':rest) -> Right (Lib1.Active, rest)
    ('F':'i':'n':'i':'s':'h':'e':'d':rest) -> Right (Lib1.Finished, rest)
    ('C':'a':'n':'c':'e':'l':'l':'e':'d':rest) -> Right (Lib1.Cancelled, rest)
    _ -> Left "Expected round status (Active|Finished|Cancelled)"

parseGameType :: Parser Lib1.GameType
parseGameType input =
  case dropWhile (== ' ') input of
    ('B':'l':'a':'c':'k':'j':'a':'c':'k':rest) -> Right (Lib1.Blackjack, rest)
    ('R':'o':'u':'l':'e':'t':'t':'e':rest) -> Right (Lib1.Roulette, rest)
    ('P':'o':'k':'e':'r':rest) -> Right (Lib1.Poker, rest)
    ('B':'a':'c':'c':'a':'r':'a':'t':rest) -> Right (Lib1.Baccarat, rest)
    ('S':'l':'o':'t':'s':rest) -> Right (Lib1.Slots, rest)
    _ -> Left "Unknown game type"

parseBetType :: Parser Lib1.BetType
parseBetType input =
  case dropWhile (== ' ') input of
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

parseBetOutcome :: Parser Lib1.BetOutcome
parseBetOutcome input =
  case dropWhile (== ' ') input of
    ('W':'i':'n':rest) -> case parseDouble (dropWhile (== ' ') rest) of
      Right (v, rest') -> Right (Lib1.Win v, rest')
      Left _ -> Left "Expected number after Win"
    ('L':'o':'s':'e':rest) -> Right (Lib1.Lose, rest)
    ('P':'u':'s':'h':rest) -> Right (Lib1.Push, rest)
    _ -> Left "Expected bet outcome (Win|Lose|Push)"


parseDump :: Parser Lib1.Command
parseDump =
  pmap (\(_, _, _) -> Lib1.Dump Lib1.Examples) $
    and3 (keyword "Dump") ws (keyword "Examples")

parseAddPlayer :: Parser Lib1.Command
parseAddPlayer =
  pmap (\(_, (_, pid, _, name, _, bal)) -> Lib1.AddPlayer pid name bal) $
    and2
      (keyword "AddPlayer")
      (and6 ws parseInt ws parseString ws parseDouble)


parseAddGame :: Parser Lib1.Command
parseAddGame =
  pmap (\(_, (_, gid, _, gname, _, gtype)) -> Lib1.AddGame gid gname gtype) $
    and2
      (keyword "AddGame")
      (and6 ws parseInt ws parseString ws parseGameType)




parseAddDealer :: Parser Lib1.Command
parseAddDealer =
  pmap (\(_, (_, did, _, dname, _, tref)) -> Lib1.AddDealer did dname tref) $
    and2
      (keyword "AddDealer")
      (and6 ws parseInt ws parseString ws parseInt)


parseAddTable :: Parser Lib1.Command
parseAddTable input =
  case keyword "AddTable" input of
    Right (_, rest0) ->
      case and5 ws parseInt ws parseString (and4 ws parseInt ws parseDouble) rest0 of
        Right ((_, tid, _, tname, (_, gref, _, minb)), rest1) ->
          case and2 ws parseDouble rest1 of
            Right ((_, maxb), rest2) ->
              let restAfterMax = dropWhile (== ' ') rest2
              in case parseInt restAfterMax of
                Right (did, rest3) -> Right (Lib1.AddTable tid tname gref minb maxb (Just did), rest3)
                Left _ -> Right (Lib1.AddTable tid tname gref minb maxb Nothing, rest2)
            Left e -> Left e
        Left e -> Left e
    Left _ -> Left "Not AddTable"

parseAddRound :: Parser Lib1.Command
parseAddRound input =
  case keyword "AddRound" input of
    Right (_, rest0) ->
      case and3 ws parseInt ws rest0 of
        Right ((_, rid, _), rest1) ->
          case parseInt rest1 of
            Right (tref, rest2) ->
              let afterTref = dropWhile (== ' ') rest2
              in case parseInt afterTref of
                Right (prid, rest3) ->
                  case and2 ws parseRoundStatus rest3 of
                    Right ((_, status), rest4) -> Right (Lib1.AddRound rid tref (Just prid) (Just status), rest4)
                    Left e -> Left e
                Left _ ->
                  case and2 ws parseRoundStatus rest2 of
                    Right ((_, status), rest3) -> Right (Lib1.AddRound rid tref Nothing (Just status), rest3)
                    Left e -> Left e
            Left e -> Left e
        Left e -> Left e
    Left _ -> Left "Not AddRound"

parsePlaceBet :: Parser Lib1.Command
parsePlaceBet input =
  case keyword "PlaceBet" input of
    Right (_, rest0) ->
      case and4 ws parseInt ws parseInt rest0 of
        Right ((_, bid, _, pref), rest1) ->
          case and4 ws parseInt ws parseDouble rest1 of
            Right ((_, tref, _, amt), rest2) ->
              case and2 ws parseBetType rest2 of
                Right ((_, btype), rest3) ->
                  let afterBType = dropWhile (== ' ') rest3
                  in if "parent" `isPrefixOf` afterBType then
                    case and3 (keyword "parent") ws parseInt afterBType of
                      Right ((_, _, pbid), rest4) ->
                        case and3 ws (keyword "round") ws rest4 of
                          Right ((_, _, _), rest5) ->
                            case parseInt rest5 of
                              Right (rref, rest6) -> Right (Lib1.PlaceBet bid pref tref amt btype (Just pbid) rref, rest6)
                              Left e -> Left e
                          Left e -> Left e
                      Left e -> Left e
                  else if "round" `isPrefixOf` afterBType then
                    case and3 (keyword "round") ws parseInt afterBType of
                      Right ((_, _, rref), rest4) -> Right (Lib1.PlaceBet bid pref tref amt btype Nothing rref, rest4)
                      Left e -> Left e
                  else
                    case parseInt afterBType of
                      Right (rref, rest4) -> Right (Lib1.PlaceBet bid pref tref amt btype Nothing rref, rest4)
                      Left e -> Left e
                Left e -> Left e
            Left e -> Left e
        Left e -> Left e
    Left _ -> Left "Not PlaceBet"

parseResolveBet :: Parser Lib1.Command
parseResolveBet =
  pmap (\(_, _, bre, _, outcome) -> Lib1.ResolveBet bre outcome) $
    and5 
      (keyword "ResolveBet") 
      ws 
      parseInt 
      ws 
      parseBetOutcome

parseDeposit :: Parser Lib1.Command
parseDeposit =
  pmap (\(_, _, pid, _, amt) -> Lib1.Deposit pid amt) $
    and5 
      (keyword "Deposit") 
      ws 
      parseInt 
      ws 
      parseDouble

parseWithdraw :: Parser Lib1.Command
parseWithdraw =
  pmap (\(_, _, pid, _, amt) -> Lib1.Withdraw pid amt) $
    and5 
      (keyword "Withdraw") 
      ws 
      parseInt 
      ws 
      parseDouble

parseSetLimit :: Parser Lib1.Command
parseSetLimit input =
  case keyword "SetLimit" input of
    Right (_, rest0) ->
      case and3 ws parseInt ws rest0 of
        Right ((_, pid, _), rest1) ->
          case dropWhile (== ' ') rest1 of
            ('D':'a':'i':'l':'y':'L':'i':'m':'i':'t':rest) ->
              case and2 ws parseDouble rest of
                Right ((_, amt), rest') -> Right (Lib1.SetLimit pid Lib1.DailyLimit amt, rest')
                Left e -> Left e
            ('W':'e':'e':'k':'l':'y':'L':'i':'m':'i':'t':rest) ->
              case and2 ws parseDouble rest of
                Right ((_, amt), rest') -> Right (Lib1.SetLimit pid Lib1.WeeklyLimit amt, rest')
                Left e -> Left e
            ('M':'o':'n':'t':'h':'l':'y':'L':'i':'m':'i':'t':rest) ->
              case and2 ws parseDouble rest of
                Right ((_, amt), rest') -> Right (Lib1.SetLimit pid Lib1.MonthlyLimit amt, rest')
                Left e -> Left e
            _ -> Left "Expected limit type"
        Left e -> Left e
    Left _ -> Left "Not SetLimit"


parseShow :: Parser Lib1.Command
parseShow input =
  case keyword "Show" input of
    Right (_, rest0) ->
      case and2 ws id rest0 of
        Right ((_, rest1), _) ->
          case dropWhile (== ' ') rest1 of
            ('P':'l':'a':'y':'e':'r':'s':rest) -> Right (Lib1.ShowPlayers, rest)
            ('G':'a':'m':'e':'s':rest) -> Right (Lib1.ShowGames, rest)
            ('T':'a':'b':'l':'e':'s':rest) -> Right (Lib1.ShowTables, rest)
            ('B':'e':'t':'s':rest) -> Right (Lib1.ShowBets, rest)
            ('R':'o':'u':'n':'d':'s':rest) -> Right (Lib1.ShowRounds, rest)
            _ -> Left "Unknown show target"
        Left e -> Left e
    Left _ -> Left "Not Show"
  where
    id x = Right (x, x)

parseRemovePlayer :: Parser Lib1.Command
parseRemovePlayer =
  pmap (\(_, _, pid) -> Lib1.RemovePlayer pid) $
    and3 
      (keyword "RemovePlayer") 
      ws 
      parseInt


parseCommand :: Parser Lib1.Command
parseCommand = foldr1 orElse
  [ parseDump
  , parseAddPlayer
  , parseAddGame
  , parseAddDealer
  , parseAddTable
  , parseAddRound
  , parsePlaceBet
  , parseResolveBet
  , parseDeposit
  , parseWithdraw
  , parseSetLimit
  , parseShow
  , parseRemovePlayer
  ]

process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "Examples:" : map toCliCommand Lib1.examples
process c = ["Parsed as " ++ show c]

class ToCliCommand a where
  toCliCommand :: a -> String

instance ToCliCommand Lib1.Command where
  toCliCommand = \case
    Lib1.Dump d -> "Dump " ++ show d
    Lib1.AddPlayer pid name bal ->
      "AddPlayer " ++ show pid ++ " \"" ++ name ++ "\" " ++ show bal
    Lib1.AddGame gid name gt ->
      "AddGame " ++ show gid ++ " \"" ++ name ++ "\" " ++ show gt
    Lib1.AddTable tid tname gref minb maxb mdr ->
      "AddTable " ++ show tid ++ " \"" ++ tname ++ "\" " ++ show gref ++ " " ++ show minb ++ " " ++ show maxb ++ maybe "" ((" " ++) . show) mdr
    Lib1.AddRound rid tref pr st ->
      let parentPart = maybe "" ((" " ++) . show) pr
          statusPart = maybe "" ((" " ++) . show) st
      in "AddRound " ++ show rid ++ " " ++ show tref ++ parentPart ++ statusPart
    Lib1.PlaceBet bid pref tref amt btype pbr rref ->
      let parentPart = maybe "" ((" parent " ++) . show) pbr
      in "PlaceBet " ++ show bid ++ " " ++ show pref ++ " " ++ show tref ++ " " ++ show amt ++ " " ++ show btype ++ parentPart ++ " round " ++ show rref
    Lib1.ResolveBet bre outcome ->
      "ResolveBet " ++ show bre ++ " " ++ show outcome
    Lib1.AddDealer did name tref ->
      "AddDealer " ++ show did ++ " \"" ++ name ++ "\" " ++ show tref
    Lib1.Deposit pid amt ->
      "Deposit " ++ show pid ++ " " ++ show amt
    Lib1.Withdraw pid amt ->
      "Withdraw " ++ show pid ++ " " ++ show amt
    Lib1.SetLimit pid ltype amt ->
      "SetLimit " ++ show pid ++ " " ++ show ltype ++ " " ++ show amt
    Lib1.ShowPlayers -> "Show Players"
    Lib1.ShowGames -> "Show Games"
    Lib1.ShowTables -> "Show Tables"
    Lib1.ShowBets -> "Show Bets"
    Lib1.ShowRounds -> "Show Rounds"
    Lib1.RemovePlayer pid -> "RemovePlayer " ++ show pid
