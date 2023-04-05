module View
  ( startApp
  ) where

import           Control                        ( HandlerInput(..)
                                                , handlerInputGame
                                                , handlerInputMainMenu
                                                , stepRandom
                                                )
import           GameLogic                      ( GameType(..)
                                                , GameValue
                                                , Player(..)
                                                , getGameValues
                                                , getRules
                                                , winnerMessage
                                                )

startApp :: IO ()
startApp = mainMenu
  [ ("Классический вариант"                  , Classic)
  , ("Колодец, камень, ножницы, бумага"      , FromFrance)
  , ("Камень, ножницы, бумага, ящерица, Спок", FromBBT)
  ]

mainMenu :: [(String, GameType)] -> IO ()
mainMenu gameTypes = do
  putStrLn "Выберите тип игры(Например: 1)"
  mapM_ (\(i, (gt, _)) -> putStrLn (show i <> ") " <> gt)) mapGameTypes
  putStrLn "q - для выхода"

  input <- handlerInputMainMenu mapGameTypes <$> getLine
  case input of
    Exit       -> pure ()
    Error  err -> putStrLn err >> mainMenu gameTypes
    Result res -> rules res

 where
  mapGameTypes :: [(Int, (String, GameType))]
  mapGameTypes = zip [1 ..] gameTypes

rules :: GameType -> IO ()
rules gameType =
  putStrLn "Правила:" >> putStrLn (getRules gameType) >> game gameType

game :: GameType -> IO ()
game gameType = do
  putStrLn "Выберите тип фигуры(Например: 1)"
  mapM_ (\(n, f) -> putStrLn $ show n <> ") " <> show f) figures
  putStrLn "q - для выхода"

  inputGameValue <- handlerInputGame figures <$> getLine
  case inputGameValue of
    Exit       -> startApp
    Error  err -> putStrLn err >> game gameType
    Result fig -> do
      putStrLn $ "Ты выбрал: " <> show fig
      figPC <- stepRandom $ snd <$> figures
      putStrLn $ "ПК выбрал: " <> show figPC
      putStrLn $ winnerMessage (Player, fig) (Pc, figPC)
      putStrLn ""

      game gameType

 where
  figures :: [(Int, GameValue)]
  figures = zip [1 ..] $ getGameValues gameType
