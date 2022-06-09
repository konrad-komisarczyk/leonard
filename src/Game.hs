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
    applyMove,
    oponent
) 
where

import qualified Data.Sequence as Seq
import Data.Maybe


-- | Parameters of the game.
data BoardSetting = BoardSetting {
    -- | Number of rows
    --   In classic Connect Four equal to 6
    nRows :: Int, 
    -- | Number of columns.
    --   In classic Connect Four equal to 7
    nColumns :: Int, 
    -- | Number of tokens placed consecutively either in horizontal, vertical, diagonal or antidiagonal line needed to win
    --   In classic Connect Four equal to 4
    lineToWin :: Int
}


-- | Type representing player
--   Either Red or Blue
data Player = Red | Blue deriving Eq


-- | State of the board of the game.
--   Sequence of elements - each of them represents an empty field ('Nothing') or a field occupied by a player's token 'Just Player'.
--   It is possible for it to store invalid states.
--   For game representation to be correct, (length 'BoardState') has to be equal m * n, where m = number of rows, n = number of columns.
--   Position (i, j) on the board is stored in the (m * i + j)th element of Sequence.
type BoardState = Seq.Seq (Maybe Player)


-- | State of the game.
--   It is possible for it to store invalid states.
data GameState = GameState {
    -- | Parameters of the game
    boardSetting:: BoardSetting,
    -- | State of the board
    boardState:: BoardState, 
    -- | Which player turn it is (who will do next move)
    player :: Player
}


-- | Move in the game. 
--   Value i means throwing token into i'th column. 
--   Columns are numbered from 0.
type Move = Int


-- | Creates new game with empty board for provided parameters.
--   Red player always has first move.
newGame :: BoardSetting -> GameState
newGame bs@(BoardSetting m n _) = GameState bs (Seq.replicate (m * n) Nothing) Red


-- | Checks whether game representation is a valid state. Checks:  
--       
--   * whether size of the board matches game parameters      
--  
--   * whether tokens in all columns are gravitated towards the bottom     
--    
--   Doesn't check if game was already won by one of the players in the past. Particularly, there can be more than one winner.
--   O(m * n)
isCorrect :: GameState -> Bool
isCorrect (GameState (BoardSetting m n k) board _) = 
    (isSizeCorrect m n k board) && (haveTokensFallen board) where
        isSizeCorrect m n k board = ((Seq.length board) == (m * n)) && (m >= k) && (n >= k) && (k > 0)
        haveTokensFallen board = all haveTokensFallenColumn (Seq.chunksOf m board) where
            haveTokensFallenColumn column = fst (foldl checkLighter (True, Just Red) column) where
                checkLighter (False, _) _ = (False, Nothing)
                checkLighter (True, Just _) a = (True, a)
                checkLighter (True, Nothing) Nothing = (True, Nothing)
                checkLighter (True, Nothing) (Just _) = (False, Nothing)


-- | Type representing position on the board. Used only for win checking.
--   1st postion is number of row, 2nd position is number of column
type Position = (Int, Int)


-- | Adds two positions. Used for moving from position into given direction.
--   Direction is one of positions (0, 1), (1, 0), (1, 1), (-1, 1)
add :: Position -> Position -> Position
add (a, b) (c, d) = (a + c, b + d)


-- | Returns 'Just element' - element of board at given position, or Nothing if the position is out of the board
--   O(log (m * n))
selectByPosition :: GameState -> Position -> Maybe (Maybe Player)
selectByPosition (GameState (BoardSetting m _ _) board _) (i, j) = 
    Seq.lookup (m * j + i) board


-- | Checks whether a given positions on the board contains token of given player
--   O(log (m * n))
checkPos :: GameState -> Player -> Position -> Bool
checkPos gameState player position =
    case selectByPosition gameState position of
        Just (Just token) -> token == player
        otherwise -> False


-- | Board positions that can be a starting position for a winning line into vertical direction
--   (m - k) * n positions
verticalStarts :: BoardSetting -> [Position]
verticalStarts (BoardSetting m n k) = [(x - 1, y - 1) | x <- [1 .. m - (k - 1)], y <- [1 .. n]]

verticalDirection :: Position
verticalDirection = (1, 0)


-- | Board positions that can be a starting position for a winning line into horizontal direction
--   m * (n - k) positions
horizontalStarts :: BoardSetting -> [Position]
horizontalStarts (BoardSetting m n k) = [(x - 1, y - 1) | x <- [1 .. m], y <- [1 .. n - (k - 1)]]

horizontalDirection :: Position
horizontalDirection = (0, 1)


-- | Board positions that can be a starting position for a winning line into diagonal direction
--   (m - k) * (n - k) positions
diagonalStarts :: BoardSetting -> [Position]
diagonalStarts (BoardSetting m n k) = [(x - 1, y - 1) | x <- [1 .. m - (k - 1)], y <- [1 .. n - (k - 1)]]

diagonalDirection :: Position
diagonalDirection = (1, 1)


-- | Board positions that can be a starting position for a winning line into antidiagonal direction
--   (m - k) * (n - k) positions
antidiagonalStarts :: BoardSetting -> [Position]
antidiagonalStarts (BoardSetting m n k) = [(x - 1, y - 1) | x <- [1 + (k - 1) .. m], y <- [1 .. n - (k - 1)]]

antidiagonalDirection :: Position
antidiagonalDirection = (-1, 1)


-- | Checks whether given position starts a winning line into given direction for a given player
--   O(k * log(m*n))
checkPosIntoDirection
    :: Position -- ^ direction
    -> Int -- ^ k (length of line to check), has to be > 0
    -> GameState -> Player 
    -> Position -- ^ starting position to check from
    -> Bool
--checkPosIntoDirection _ 0 _ _ _ = False -- Should never happen with correct k > 0
checkPosIntoDirection _ 1 gs player position = checkPos gs player position
checkPosIntoDirection direction k gs player position = 
    (checkPos gs player position) && (checkPosIntoDirection direction (k - 1) gs player (add position direction))


-- | Checks whether one of given positions starts a winning line into given direction for a given player
--   O(p * k * log(m*n)), where p is number of positions to check
checkPositions :: GameState -> Player -> Position -> [Position] -> Bool
checkPositions gs@(GameState (BoardSetting _ _ k) _ _) player direction = any (checkPosIntoDirection direction k gs player)


-- | Returns whether given player has a winning line
--   O((2(m-k)(n-k) + m(n-k) + n(m-k)) * k * log(m*n))
doesPlayerWin :: GameState -> Player -> Bool
doesPlayerWin gs@(GameState bs _ _) player =
    (checkPositions gs player horizontalDirection (horizontalStarts bs)) ||
    (checkPositions gs player verticalDirection (verticalStarts bs)) ||
    (checkPositions gs player diagonalDirection (diagonalStarts bs)) ||
    (checkPositions gs player antidiagonalDirection (antidiagonalStarts bs))


-- | Returns one of the winners of a given game.
--   'Nothing' if there is no winnner yet
--   'Just Player' - one of the players having winning position
--   For performance purposes doesn't check if game state is correct.
--   If game wasn't played correctly, there can be more than one winner but always only one of them is returned.
--   O(2 * (2(m-k)(n-k) + m(n-k) + n(m-k)) * k * log(m*n))
whoWins :: GameState -> Maybe Player
whoWins gameState
    | doesPlayerWin gameState Red = Just Red
    | doesPlayerWin gameState Blue = Just Blue
    | otherwise = Nothing


-- | Returns list of available moves for a given game.
--   Available moves are columns that have their top field empty.
--   For performance purposes doesn't check correctness of the game.
--   O(n * log(m + n))
availableMoves :: GameState -> [Move]
availableMoves (GameState (BoardSetting m n k) board _) = 
    filter (\column -> Data.Maybe.isNothing (Seq.index board (m * column + m - 1))) [0 .. n - 1]


-- | For given player a player that will move next.
oponent :: Player -> Player
oponent Red = Blue
oponent Blue = Red


-- | For a given game state and a move plays that move and returns resulting game state if move was possible.
--   O (m + n * log(m))
applyMove 
    :: GameState -- ^ A game state. Correctness is not checked for performance reasons.
    -> Move -- ^ Number of column in which the token will be placed.
    -> Maybe GameState -- ^ If chosen column has at least one free place, then 'Just GameState' - game state with the move played. 
    --   Token will be placed on the lowest free place int the column. Token's color will corespond to the player whose turn it was.
    --   If chosen column was full, or there was not enough columns, then 'Nothing'.
applyMove gs@(GameState bs@(BoardSetting m n k) board player) move =
    case firstFreeFromColumn of
            Just firstFree -> Just (GameState bs (Seq.adjust' (\_ -> Just player) (m * move + firstFree) board) (oponent player))
            Nothing -> Nothing
    where
        firstFreeFromColumn = Seq.findIndexL Data.Maybe.isNothing (Seq.index (Seq.chunksOf m board) move)
