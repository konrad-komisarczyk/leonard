module AI where

import Game
import System.Random

type Hiperparameters = Int

getNextMove :: RandomGen g => GameState -> Hiperparameters -> g -> (Maybe Move, g)
getNextMove gameState _ generator =
    (Just (moves !! rand), nextGenerator)
    where
        moves = Game.availableMoves gameState
        (rand, nextGenerator) = randomR (0, ((length moves) - 1)) generator
            
        