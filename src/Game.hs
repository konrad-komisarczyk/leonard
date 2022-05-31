module Game
(
    BoardSetting (..),
    Player (..),
    BoardState (..),
    GameState (..),
    Move (..),
    newGame,
    whoWins,
    availableMoves, 
    applyMove
) 
where

import qualified Data.Sequence as Seq
import Data.Maybe


data BoardSetting = BoardSetting {nRows :: Int, nColumns :: Int, lineToWin :: Int}

data Player = Red | Blue deriving Eq

type BoardState = Seq.Seq (Maybe Player)

data GameState = GameState BoardSetting BoardState Player

type Move = Int


newGame :: BoardSetting -> GameState
newGame bs@(BoardSetting m n _) = GameState bs (Seq.replicate (m * n) Nothing) Red


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


positionToElement :: (Int, Int) -> Int -> Int
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

doesPlayerWin :: GameState -> Player -> Bool
doesPlayerWin gs@(GameState (BoardSetting m n k) b _) player = 
    (checkAllWithinSquareDirection (0, 0) (m - 1, n - k) (0, 1) (k - 1) gs player) || -- horizontal
    (checkAllWithinSquareDirection (0, 0) (m - k, n - 1) (1, 0) (k - 1) gs player) || -- vertical
    (checkAllWithinSquareDirection (0, 0) (m - k, n - k) (1, 1) (k - 1) gs player) || -- diagonal
    (checkAllWithinSquareDirection (0, k - 1) (m - k, n - 1) (1, -1) (k - 1) gs player) -- antidiagonal

whoWins :: GameState -> Maybe Player
whoWins gameState
    | doesPlayerWin gameState Red = Just Red
    | doesPlayerWin gameState Blue = Just Blue
    | otherwise = Nothing


availableMoves :: GameState -> [Move]
availableMoves (GameState (BoardSetting m n k) board _) = 
    Seq.findIndicesL (any Data.Maybe.isNothing) (Seq.chunksOf m board)

oponent :: Player -> Player
oponent Red = Blue
oponent Blue = Red



applyMove :: GameState -> Move -> Maybe GameState
applyMove gs@(GameState bs@(BoardSetting m n k) board player) move =
    case firstFreeFromColumn of
            Just firstFree -> Just (GameState bs (Seq.adjust' (\_ -> Just player) (m * move + firstFree) board) (oponent player))
            Nothing -> Nothing
    where
        firstFreeFromColumn = Seq.findIndexL Data.Maybe.isNothing (Seq.index (Seq.chunksOf m board) move)

