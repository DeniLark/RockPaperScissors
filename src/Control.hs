module Control
  ( handlerInputMainMenu
  , handlerInputGame
  , stepRandom
  , HandlerInput(..)
  ) where

import           System.Random                  ( Random(randomR)
                                                , newStdGen
                                                )

import           GameLogic                      ( GameType
                                                , GameValue
                                                )

data HandlerInput a = Exit
                    | Error String
                    | Result a

handlerInputMainMenu
  :: [(Int, (String, GameType))] -> String -> HandlerInput GameType
handlerInputMainMenu _         "q"   = Exit
handlerInputMainMenu gameTypes input = case reads input :: [(Int, String)] of
  [] -> Error "Недопустимый ввод"
  (num, _) : _ ->
    maybe (Error "Недопустимый ввод") (Result . snd) $ lookup num gameTypes

handlerInputGame :: [(Int, GameValue)] -> String -> HandlerInput GameValue
handlerInputGame _          "q"   = Exit
handlerInputGame gameValues input = case reads input :: [(Int, String)] of
  [] -> Error "Недопустимый ввод"
  (num, _) : _ ->
    maybe (Error "Недопустимый ввод") Result $ lookup num gameValues

stepRandom :: [GameValue] -> IO GameValue
stepRandom gameValues =
  (gameValues !!) . fst . randomR (0, length gameValues - 1) <$> newStdGen
