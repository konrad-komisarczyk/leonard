
{-|
Module      : AI
Description : Implementation of AI player based on the MCTS algorithm
-}
module AI (
    Hiperparameters (..),
    Generator (..),
    newGenerator,
    getNextMove
) where

import Game
import System.Random

-- | Type representing MCTS algorithm hiperparameters
type Hiperparameters = Int

-- | Random number generator state used by the AI algorithm.
type Generator = StdGen

-- | For a given seed returns new generator.
newGenerator :: Int -> Generator
newGenerator = mkStdGen

-- | Returns a completly random move when only there is one possible.
getRandomMove :: RandomGen g => GameState -> g -> (Maybe Move, g)
getRandomMove gameState generator =
    (Just (moves !! rand), nextGenerator)
    where
        moves = Game.availableMoves gameState
        (rand, nextGenerator) = randomR (0, ((length moves) - 1)) generator

-- | Returns next move of the AI player.
getNextMove :: RandomGen g 
    -- | Game state to which AI player should respond
    => GameState 
    -- | MCTS algorithm hiperparameters
    -> Hiperparameters 
    -- | Random number generator state
    -> g 
    -- | If there is a move possible then Just Move, if there is no move possible Nothing
    -> (Maybe Move, 
    -- | State of the random number generator after calculating the move.
    g)
getNextMove gameState _ generator =
    getRandomMove gameState generator
    
            
        