{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module GameConstants where

import Game
import AI
import Data.Text
import Data.Vector

type Difficulty = Int

defaultDifficulty :: Difficulty
defaultDifficulty = 0

difficultyNames :: Vector Text
difficultyNames = ["Random", "Beginner", "Intermediate", "Advanced"]

maxDifficulty :: Difficulty
maxDifficulty = (Data.Vector.length difficultyNames) - 1

minDifficulty :: Difficulty
minDifficulty = 0

difficultyName :: Difficulty -> Text
difficultyName d = difficultyNames Data.Vector.! d

difficultyToHiperparameters :: Difficulty -> AI.Hiperparameters
difficultyToHiperparameters _ = 1

defaultBoardSetting :: BoardSetting
defaultBoardSetting = BoardSetting 6 7 4

maxRows :: Int
maxRows = 11

maxColumns :: Int
maxColumns = 19

minLine :: Int
minLine = 3