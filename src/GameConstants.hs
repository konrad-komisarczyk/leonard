{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-|
Module      : GameConstants
Description : Constants for the application that are connected to the game, not the application appearance
-}
module GameConstants where

import Game
import AI
import Data.Text
import Data.Vector
import Data.Int


-- DIFFICULTY CONSTANTS --

-- | Int alias representing Computer player difficulty.
type Difficulty = Int


defaultDifficulty :: Difficulty
defaultDifficulty = 1


-- | Difficulty level names to display. Vectors i'th element is name of difficulty = i.
difficultyNames :: Vector Text
difficultyNames = ["Random", "Beginner", "Intermediate", "Advanced"]


difficultyName :: Difficulty -> Text
difficultyName d = difficultyNames Data.Vector.! d


maxDifficultyNameLength :: Int32
maxDifficultyNameLength = 12


-- | Difficulty levels range upper bound.
maxDifficulty :: Difficulty
maxDifficulty = (Data.Vector.length difficultyNames) - 1


-- | Difficulty levels range lower bound.
minDifficulty :: Difficulty
minDifficulty = 0


difficultyToHiperparameters :: Difficulty -> AI.Hiperparameters
difficultyToHiperparameters 0 = Random
difficultyToHiperparameters 1 = MCTS (sqrt 2) 4000
difficultyToHiperparameters 2 = MCTS (sqrt 2) 8000
difficultyToHiperparameters 3 = MCTS (sqrt 2) 24000


-- BOARD SETTING CONSTANTS --

defaultBoardSetting :: BoardSetting
defaultBoardSetting = BoardSetting 6 7 4


maxRows :: Int
maxRows = 11


maxColumns :: Int
maxColumns = 19


minLine :: Int
minLine = 3
