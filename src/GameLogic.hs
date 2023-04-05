{-# LANGUAGE LambdaCase #-}

module GameLogic
  ( winnerMessage
  , GameType(..)
  , GameValue(..)
  , getRules
  , getGameValues
  , Player(..)
  ) where


data GameType = Classic     -- классический вариант 
              | FromFrance  -- колодец, камень, ножницы, бумага 
              | FromBBT     -- камень, ножницы, бумага, ящерица, Спок

getRules :: GameType -> String
getRules Classic = unlines
  [ "Бумага  побеждает камень"
  , "Камень  побеждает ножницы"
  , "Ножницы побеждают бумагу"
  ]
getRules FromFrance = unlines
  [ "Бумага  побеждает камень и колодец"
  , "Камень  побеждает ножницы"
  , "Ножницы побеждают бумагу"
  , "Колодец побеждает ножницы и камень"
  ]
getRules FromBBT = unlines
  [ "Бумага  побеждает камень и Спока"
  , "Камень  побеждает ножницы и ящерицу"
  , "Ножницы побеждают бумагу и ящерицу"
  , "Ящерица побеждает Спока и бумагу"
  , "Спок    побеждает ножницы и камень"
  ]

getGameValues :: GameType -> [GameValue]
getGameValues Classic    = [Rock, Scissors, Paper]
getGameValues FromFrance = [Well, Rock, Scissors, Paper]
getGameValues FromBBT    = [Rock, Scissors, Paper, Lizard, Spock]

data GameValue = Rock | Paper | Scissors | Well | Lizard | Spock
  deriving Eq

instance Show GameValue where
  show Rock     = "Камень"
  show Paper    = "Бумага"
  show Scissors = "Ножницы"
  show Well     = "Колодец"
  show Lizard   = "Ящерица"
  show Spock    = "Спок"


data Player = Player | Pc

winnerMessage :: (Player, GameValue) -> (Player, GameValue) -> String
winnerMessage a =
  maybe
      "Ничья"
      (\case
        Player -> "Победитель: ты"
        Pc     -> "Победитель: ПК"
      )
    . winner a

winner :: (Player, GameValue) -> (Player, GameValue) -> Maybe Player
winner (a1, gw1) (a2, gw2) = helper <$> winnerGameValue gw1 gw2
 where
  helper :: GameValue -> Player
  helper gw | gw == gw1 = a1
            | otherwise = a2

winnerGameValue :: GameValue -> GameValue -> Maybe GameValue
winnerGameValue Paper    Rock     = Just Paper
winnerGameValue Rock     Scissors = Just Rock
winnerGameValue Scissors Paper    = Just Scissors
winnerGameValue a b | a == b    = Nothing
                    | otherwise = winnerGameValue b a

