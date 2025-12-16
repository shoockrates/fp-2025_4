{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lib1
  ( keywords,
    examples,
    Command (..),
    Dumpable (..),
    GameType (..),
    BetType (..),
    BetOutcome (..),
    RoundStatus (..),
    LimitType (..),
  )
where

data Dumpable = Examples
  deriving (Show, Read)

-- Manual Eq instance for Dumpable
instance Eq Dumpable where
  Examples == Examples = True

keywords :: [String]
keywords =
  [ "add",
    "player",
    "game",
    "table",
    "bet",
    "place",
    "resolve",
    "win",
    "lose",
    "show",
    "players",
    "games",
    "tables",
    "bets",
    "remove",
    "dump",
    "examples",
    "parent",
    "round",
    "deposit",
    "withdraw",
    "set",
    "limit",
    "dealer",
    "balance",
    "amount",
    "type",
    "status"
  ]

data Command
  = AddPlayer
      { playerId :: Integer,
        playerName :: String,
        initialBalance :: Double
      }
  | AddGame
      { gameId :: Integer,
        gameName :: String,
        gameType :: GameType
      }
  | AddTable
      { tableId :: Integer,
        tableName :: String,
        gameRef :: Integer,
        minBet :: Double,
        maxBet :: Double,
        dealerRef :: Maybe Integer
      }
  | PlaceBet
      { betId :: Integer,
        playerRef :: Integer,
        tableRef :: Integer,
        amount :: Double,
        betType :: BetType,
        parentBetId :: Maybe Integer,
        roundRef :: Integer
      }
  | AddRound
      { roundId :: Integer,
        tableRef :: Integer,
        parentRoundId :: Maybe Integer,
        status :: Maybe RoundStatus
      }
  | ResolveBet
      { betRef :: Integer,
        outcome :: BetOutcome
      }
  | AddDealer
      { dealerId :: Integer,
        dealerName :: String,
        tableRef :: Integer
      }
  | Deposit
      { playerRef :: Integer,
        amount :: Double
      }
  | Withdraw
      { playerRef :: Integer,
        amount :: Double
      }
  | SetLimit
      { playerRef :: Integer,
        limitType :: LimitType,
        amount :: Double
      }
  | ShowPlayers
  | ShowGames
  | ShowTables
  | ShowBets
  | ShowRounds
  | RemovePlayer
      { playerId :: Integer
      }
  | Dump Dumpable
  deriving (Show, Read)

-- Manual Eq instance for Command
instance Eq Command where
  (AddPlayer pid1 n1 b1) == (AddPlayer pid2 n2 b2) = 
    pid1 == pid2 && n1 == n2 && b1 == b2
  (AddGame gid1 gn1 gt1) == (AddGame gid2 gn2 gt2) = 
    gid1 == gid2 && gn1 == gn2 && gt1 == gt2
  (AddTable tid1 tn1 gr1 minb1 maxb1 dr1) == (AddTable tid2 tn2 gr2 minb2 maxb2 dr2) = 
    tid1 == tid2 && tn1 == tn2 && gr1 == gr2 && minb1 == minb2 && maxb1 == maxb2 && dr1 == dr2
  (PlaceBet bid1 pr1 tr1 a1 bt1 pb1 rr1) == (PlaceBet bid2 pr2 tr2 a2 bt2 pb2 rr2) = 
    bid1 == bid2 && pr1 == pr2 && tr1 == tr2 && a1 == a2 && bt1 == bt2 && pb1 == pb2 && rr1 == rr2
  (AddRound rid1 tr1 pr1 s1) == (AddRound rid2 tr2 pr2 s2) = 
    rid1 == rid2 && tr1 == tr2 && pr1 == pr2 && s1 == s2
  (ResolveBet br1 o1) == (ResolveBet br2 o2) = 
    br1 == br2 && o1 == o2
  (AddDealer did1 dn1 tr1) == (AddDealer did2 dn2 tr2) = 
    did1 == did2 && dn1 == dn2 && tr1 == tr2
  (Deposit pr1 a1) == (Deposit pr2 a2) = 
    pr1 == pr2 && a1 == a2
  (Withdraw pr1 a1) == (Withdraw pr2 a2) = 
    pr1 == pr2 && a1 == a2
  (SetLimit pr1 lt1 a1) == (SetLimit pr2 lt2 a2) = 
    pr1 == pr2 && lt1 == lt2 && a1 == a2
  ShowPlayers == ShowPlayers = True
  ShowGames == ShowGames = True
  ShowTables == ShowTables = True
  ShowBets == ShowBets = True
  ShowRounds == ShowRounds = True
  (RemovePlayer pid1) == (RemovePlayer pid2) = pid1 == pid2
  (Dump d1) == (Dump d2) = d1 == d2
  _ == _ = False

data GameType = Blackjack | Roulette | Poker | Baccarat | Slots
  deriving (Show, Read, Eq)

data BetType = Straight | Split | Corner | Red | Black | Odd | Even | Pass | DontPass
  deriving (Show, Read, Eq)

data BetOutcome = Win Double | Lose | Push
  deriving (Show, Read, Eq)

data RoundStatus = Active | Finished | Cancelled
  deriving (Show, Read, Eq)

data LimitType = DailyLimit | WeeklyLimit | MonthlyLimit
  deriving (Show, Read, Eq)

examples :: [Command]
examples =
  [ AddPlayer 1 "John Smith" 1000.0,
    AddGame 1 "European Roulette" Roulette,
    AddDealer 1 "Maria Garcia" 1,
    AddTable 1 "High Roller Roulette" 1 100.0 5000.0 (Just 1),
    AddRound 1 1 Nothing (Just Active),
    PlaceBet 1 1 1 500.0 Red Nothing 1,
    AddRound 2 1 (Just 1) (Just Active),
    PlaceBet 2 1 1 100.0 Odd (Just 1) 2,
    PlaceBet 3 1 1 200.0 Straight (Just 1) 1,
    ResolveBet 1 (Win 1000.0),
    ResolveBet 2 (Win 200.0),
    ResolveBet 3 Lose,
    Deposit 1 2000.0,
    SetLimit 1 DailyLimit 10000.0,
    Withdraw 1 500.0,
    RemovePlayer 1,
    Dump Examples
  ]
