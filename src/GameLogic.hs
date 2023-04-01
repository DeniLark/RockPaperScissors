{-# LANGUAGE LambdaCase #-}

module GameLogic
  ( winnerMessage
  ) where

data GameValue = Rock | Paper | Scissors
  deriving (Eq, Show)

data Player = Player | Pc

winnerMessage :: (Player, GameValue) -> (Player, GameValue) -> String
winnerMessage a =
  maybe
      "Ничья"
      (\case
        Player -> "Победитель: ты"
        Pc     -> "Победитель: случай"
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

{-
  1. Бумага побеждает камень  
     Камень побеждает ножницы 
     Ножницы побеждают бумагу 
-}
