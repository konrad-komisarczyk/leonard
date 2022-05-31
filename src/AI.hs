module AI where

import Game
import System.Random

type Difficulty = Int

defaultDifficulty :: Difficulty
defaultDifficulty = 1

getNextMove :: RandomGen g => GameState -> Difficulty -> g -> (Maybe Move, g)
getNextMove gameState _ generator =
    (Just (moves !! rand), nextGenerator)
    where
        moves = Game.availableMoves gameState
        (rand, nextGenerator) = randomR (0, ((length moves) - 1)) generator
            
        