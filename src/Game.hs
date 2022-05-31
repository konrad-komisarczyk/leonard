{-|
Module      : Game
Description : Connect Four game representation

Declarative representation of generalized Connect Four game for two players.
Can represent any game as series of applications of appropiate functions.
Game can be parametrized with number of rows, columns, and line length needed to win. 
Exported functions allow to create game, play it (check available moves and play moves) and check who wins. 
-}
module Game
(
    BoardSetting (..),
    Player (..),
    BoardState (..),
    GameState (..),
    Move (..),
    newGame,
    doesPlayerWin,
    whoWins,
    availableMoves, 
    applyMove
) 
where

import qualified Data.Sequence as Seq
import Data.Maybe

-- | Parameters of the game.
data BoardSetting = BoardSetting {
    -- | Number of rows
    -- | In classic Connect Four equal to 6
    nRows :: Int, 
    -- | Number of columns.
    -- | In classic Connect Four equal to 7
    nColumns :: Int, 
    -- | Number of tokens placed consecutively either in horizontal, vertical, diagonal or antidiagonal line needed to win
    -- | In classic Connect Four equal to 4
    lineToWin :: Int
}

-- | Type representing player
-- | Either Red or Blue
data Player = Red | Blue deriving Eq

-- | State of the board of the game.
-- | Sequence of elements - each of them represents an empty field ('Nothing') or a field occupied by a player's token 'Just Player'.
-- | It is possible for it to store invalid states.
-- | For game representation to be correct, (length 'BoardState') has to be equal m * n, where m = number of rows, n = number of columns.
-- | Position (i, j) on the board is stored in the (m * i + j)th element of Sequence.
type BoardState = Seq.Seq (Maybe Player)

-- | State of the game.
-- | It is possible for it to store invalid states.
data GameState = GameState {
    -- | Parameters of the game
    boardSetting:: BoardSetting,
    -- | State of the board
    boardState:: BoardState, 
    -- | Which player turn it is (who will do next move)
    player :: Player
}

-- | Move in the game. 
-- | Value 'i' means throwing token into i'th column. 
-- | Columns are numbered from 0.
type Move = Int

-- | Creates new game with empty board for provided parameters.
-- | Red player always has first move.
newGame :: BoardSetting -> GameState
newGame bs@(BoardSetting m n _) = GameState bs (Seq.replicate (m * n) Nothing) Red


-- | Checks whether game representation is a valid state. Checks:
-- | * whether size of the board matches game parameters
-- | * whether tokens in all columns are gravitated towards the bottom
-- | Doesn't check if game was already won by one of the players in the past. Particularly, there can be more than one winner.
isCorrect :: GameState -> Bool
isCorrect (GameState (BoardSetting m n k) board _) = 
    (isSizeCorrect m n k board) && (haveTokensFallen board) where
        isSizeCorrect m n k board = ((Seq.length board) == (m * n)) && (m >= k) && (n >= k)
        haveTokensFallen board = all haveTokensFallenColumn (Seq.chunksOf m board) where
            haveTokensFallenColumn column = fst (foldl checkLighter (True, Just Red) column) where
                checkLighter (False, _) _ = (False, Nothing)
                checkLighter (True, Just _) a = (True, a)
                checkLighter (True, Nothing) Nothing = (True, Nothing)
                checkLighter (True, Nothing) (Just _) = (False, Nothing)


-- | Maps position on the board to the position in BoardState Sequence.
positionToElement 
    -- | Position on the board.
    :: (Int, Int)
    -- | Number of rows in the game 
    -> Int 
    -- | Position in the Sequence.
    -> Int
positionToElement (i, j) m = m * i + j

checkPosition :: (Int, Int) -> GameState -> Player -> Bool
checkPosition position (GameState (BoardSetting m n _) board _) player = 
    case Seq.lookup (positionToElement position m) board of
        Nothing -> False
        Just x -> (x == (Just player))

checkPositionsDirection :: (Int, Int) -> Int -> (Int, Int) -> GameState -> Player -> Bool
checkPositionsDirection _ 0 position gs player = checkPosition position gs player
checkPositionsDirection (v, h) k (i, j) gs player = 
    (checkPosition (i + v, j + h) gs player) && (checkPositionsDirection (v, h) (k - 1) (i + v, j + h) gs player)

checkAllWithinSquareDirection :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> GameState -> Player -> Bool
checkAllWithinSquareDirection (down, left) (up, right) direction k gs player
    | left >= right = False
    | otherwise = 
         (checkAllWithinColumnpartDirection left down up direction k gs player) || (checkAllWithinSquareDirection (down, left + 1) (up, right) direction k gs player) 
     where
        checkAllWithinColumnpartDirection column down up direction k gs player
            | down >= up = False
            | otherwise = (checkPositionsDirection direction k (column, down) gs player) || (checkAllWithinColumnpartDirection column (down + 1) up direction k gs player)

-- | Checks whether game state is won for a given player
doesPlayerWin :: GameState -> Player -> Bool
doesPlayerWin gs@(GameState (BoardSetting m n k) b _) player = 
    (checkAllWithinSquareDirection (0, 0) (m - 1, n - k) (0, 1) (k - 1) gs player) || -- horizontal
    (checkAllWithinSquareDirection (0, 0) (m - k, n - 1) (1, 0) (k - 1) gs player) || -- vertical
    (checkAllWithinSquareDirection (0, 0) (m - k, n - k) (1, 1) (k - 1) gs player) || -- diagonal
    (checkAllWithinSquareDirection (0, k - 1) (m - k, n - 1) (1, -1) (k - 1) gs player) -- antidiagonal

-- | Returns one of the winners of a given game.
-- | 'Nothing' if there is no winnner yet
-- | 'Just Player' - one of the players having winning position
-- | For performance purposes doesn't check if game state is correct.
-- | If game wasn't played correctly, there can be more than one winner but always only one of them is returned.
whoWins :: GameState -> Maybe Player
whoWins gameState
    | doesPlayerWin gameState Red = Just Red
    | doesPlayerWin gameState Blue = Just Blue
    | otherwise = Nothing

-- | Returns list of available moves for a given game.
-- | Available moves are columns that are not full.
-- | For performance purposes doesn't check correctness of the game.
availableMoves :: GameState -> [Move]
availableMoves (GameState (BoardSetting m n k) board _) = 
    Seq.findIndicesL (any Data.Maybe.isNothing) (Seq.chunksOf m board)

-- | For given player a player that will move next.
oponent :: Player -> Player
oponent Red = Blue
oponent Blue = Red


-- | For a given game state and a move plays that move and returns resulting game state if move was possible.
applyMove 
    -- | A game state. Correctness is not checked for performance reasons.
    :: GameState 
    -- | Number of column in which the token will be placed.
    -> Move 
    -- | If chosen column has at least one free place, then 'Just GameState' - game state with the move played. 
    -- | Token will be placed on the lowest free place int the column. Token's color will corespond to the player whose turn it was.
    -- | If chosen column was full, or there was not enough columns, then 'Nothing'.
    -> Maybe GameState
applyMove gs@(GameState bs@(BoardSetting m n k) board player) move =
    case firstFreeFromColumn of
            Just firstFree -> Just (GameState bs (Seq.adjust' (\_ -> Just player) (m * move + firstFree) board) (oponent player))
            Nothing -> Nothing
    where
        firstFreeFromColumn = Seq.findIndexL Data.Maybe.isNothing (Seq.index (Seq.chunksOf m board) move)

