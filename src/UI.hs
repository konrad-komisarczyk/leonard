{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module UI(
    State (..), 
    Event (..), 
    view', 
    update', 
    initialState'
) where

import Game
import GameConstants
import AI
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import GI.Gtk.Declarative.Container.Grid
import qualified Data.Sequence as Seq
import Data.Text (Text, pack)
import Data.Vector (Vector, fromList, (++))
import Data.Foldable (toList)
import Data.Int
import Data.Maybe
import System.Random
import Control.Concurrent (threadDelay)
import Data.Functor (($>))

type PlaySpeed = Int

defaultSpeed :: PlaySpeed
defaultSpeed = 1000000 -- 1s

data AISetting = AISetting {hiperparameters :: AI.Hiperparameters, generator :: StdGen}


data State 
    = SettingsWindow {
        boardSetting :: Game.BoardSetting, 
        isRedComputer :: Bool, 
        redDifficulty :: GameConstants.Difficulty, 
        redSeed :: Int,
        isBlueComputer :: Bool, 
        blueDifficulty :: GameConstants.Difficulty,
        blueSeed :: Int}
    | UserVSUserWindow {
        gameState :: Game.GameState} 
    | UserVSComputerWindow {
        gameState :: Game.GameState, 
        computerColor :: Game.Player, 
        computerSetting :: AISetting} 
    | ComputerVSComputerWindow {
        gameState :: Game.GameState, 
        red :: AISetting, 
        blue :: AISetting, 
        speed :: PlaySpeed, 
        paused :: Bool}
    | ErrorWindow {
        message :: Text}

data Event 
    = Closed 
    | Error Text
    | StartGame | EndGame 
    | MovePlayed Game.Move | AwaitingComputerMove
    | SimulationPaused | SimulationResumed
    | MChanged Int | NChanged Int | KChanged Int
    | RedTypeChanged Bool 
    | BlueTypeChanged Bool 
    | RedDifficultyChanged GameConstants.Difficulty
    | BlueDifficultyChanged GameConstants.Difficulty
    | RedSeedChanged Int
    | BlueSeedChanged Int

defaultSettingsWindow :: State
defaultSettingsWindow = SettingsWindow GameConstants.defaultBoardSetting False GameConstants.defaultDifficulty 137 False GameConstants.defaultDifficulty 213

mainWrapper :: Widget Event -> AppView Gtk.Window Event
mainWrapper child = bin Gtk.Window
  [ #title := "λeonard", 
  on #deleteEvent (const (True, Closed)), 
  #resizable := False
  ]
  $ child

plusButton :: Int -> Int -> (Int -> Event) -> BoxChild Event
plusButton currVal maxVal event = 
    widget Gtk.Button [
        #label := "+", 
        on #clicked (event (currVal + 1)), 
        #sensitive := (currVal < maxVal)
        ]

minusButton :: Int -> Int -> (Int -> Event) -> BoxChild Event
minusButton currVal minVal event = 
    widget Gtk.Button [
        #label := "-", 
        on #clicked (event (currVal - 1)), 
        #sensitive := (currVal > minVal)
        ]



boardElemToImage :: Maybe Player -> Widget Event
boardElemToImage Nothing = widget Gtk.Image [#file := "img/empty.png"]
boardElemToImage (Just Red) = widget Gtk.Image [#file := "img/red.png"]
boardElemToImage (Just Blue) = widget Gtk.Image [#file := "img/blue.png"]

boardElemToGridChild :: Int -> Int -> Maybe Player -> GridChild Event
boardElemToGridChild m index elem = GridChild {
    properties = defaultGridChildProperties {
        width = 1, height = 1, 
        leftAttach = fromIntegral (div index m), 
        topAttach = (fromIntegral (m - (mod index m))) + 1
        },
    child = (boardElemToImage elem)
}

boardToGridChildren :: Int -> BoardState -> Vector (GridChild Event)
boardToGridChildren m board = fromList (toList (Seq.mapWithIndex (boardElemToGridChild m) board))

moveToButton :: Player -> Move -> Widget Event
moveToButton Red move = bin Gtk.Button [on #clicked (MovePlayed move), #margin := 4] (widget Gtk.Image [#file := "img/redArrow.png"])
moveToButton Blue move = bin Gtk.Button [on #clicked (MovePlayed move), #margin := 4] (widget Gtk.Image [#file := "img/blueArrow.png"])

moveToGridChild :: Player -> Move -> GridChild Event
moveToGridChild player move = GridChild {
    properties = defaultGridChildProperties {
        width = 1, height = 1,
        leftAttach = ((fromIntegral move) :: Int32), 
        topAttach = 0
        },
    child = (moveToButton player move)
}

movesToGridChildren :: Player -> [Move] -> Vector (GridChild Event)
movesToGridChildren player moves = fromList (map (moveToGridChild player) moves)


gameGrid :: GameState -> Bool -> BoxChild Event
gameGrid gameState@(GameState bs@(BoardSetting m n _) board player) showMoves = container Gtk.Grid [
    #rowSpacing := 0,
    #columnSpacing := 0
    ] gridElements where
        gridElements
            | showMoves = (boardToGridChildren m board) Data.Vector.++ (movesToGridChildren player (Game.availableMoves gameState))
            | otherwise = boardToGridChildren m board

winInformation :: Maybe Player -> Vector (BoxChild Event)
winInformation Nothing = []
winInformation (Just Blue) = [widget Gtk.Label [#label := "Blue player wins!", classes ["winInformation", "blue"]]]
winInformation (Just Red) = [widget Gtk.Label [#label := "Red player wins!", classes ["winInformation", "red"]]]

pauseButton :: Maybe Bool -> Vector (BoxChild Event)
pauseButton Nothing = []
pauseButton (Just True) = [widget Gtk.Button [#label := "Resume", on #clicked SimulationResumed]]
pauseButton (Just False) = [widget Gtk.Button [#label := "Pause", on #clicked SimulationPaused]]

gameWindow :: GameState -> Bool -> Maybe Player -> Maybe Bool -> AppView Gtk.Window Event
gameWindow gameState showMoves winningPlayer maybePaused = mainWrapper
  $ container Gtk.Box [
      #orientation := Gtk.OrientationVertical, 
      #halign := Gtk.AlignCenter] [
      container Gtk.Box [
          #orientation := Gtk.OrientationVertical, 
          #halign := Gtk.AlignCenter,
          #margin := 10
          ] (winInformation winningPlayer),
      container Gtk.Box [
          #orientation := Gtk.OrientationVertical, 
          #halign := Gtk.AlignCenter,
          #margin := 10
          ] (pauseButton maybePaused),
      container Gtk.Box [
          #orientation := Gtk.OrientationHorizontal
          ] [
          container Gtk.Box [ -- board container
              #orientation := Gtk.OrientationVertical,
              #margin := 10
              ] [
              (gameGrid gameState showMoves)
          ]
      ],
      container Gtk.Box [
          #orientation := Gtk.OrientationVertical,
          #margin := 10] [
          widget Gtk.Button [#label := "Back to Main Menu", on #clicked EndGame, #halign := Gtk.AlignCenter]
      ]
  ]


view' :: State -> AppView Gtk.Window Event
-- Error window
view' (ErrorWindow message) = 
    mainWrapper $ widget Gtk.Label [#label := message]
-- Settings window
view' (SettingsWindow boardSetting@(BoardSetting m n k) isRedComputer redD redSeed isBlueComputer blueD blueSeed) = 
    mainWrapper $ container Gtk.Box [
      #orientation := Gtk.OrientationVertical,
      #margin := 10
      ] [
       -- first row (board settings)
      widget Gtk.Label [
          #label := "Board settings: ", 
          #halign := Gtk.AlignCenter,
          #marginBottom := 4, 
          classes ["header"]],
      container Gtk.Box [
          #orientation := Gtk.OrientationVertical
          ] [
          container Gtk.Box [ -- rows settings
              #marginTop := 8,
              #spacing := 8
          ] [
              widget Gtk.Label [#label := "Number of rows: ", classes ["settingsLabel"]],
              minusButton m k (\a -> MChanged a),
              widget Gtk.Label [#label := (Data.Text.pack (show m))],
              plusButton m GameConstants.maxRows (\a -> MChanged a)
          ],
          container Gtk.Box [ -- columns settings
              #marginTop := 8,
              #spacing := 8
          ] [
              widget Gtk.Label [#label := "Number of columns: ", classes ["settingsLabel"]],
              minusButton n k (\a -> NChanged a),
              widget Gtk.Label [#label := (Data.Text.pack (show n))],
              plusButton n GameConstants.maxColumns (\a -> NChanged a)
          ],
          container Gtk.Box [ -- winning line settings
              #marginTop := 8,
              #spacing := 8
          ] [
              widget Gtk.Label [#label := "Length of line to win: ", classes ["settingsLabel"]],
              minusButton k GameConstants.minLine (\a -> KChanged a),
              widget Gtk.Label [#label := (Data.Text.pack (show k))],
              plusButton k (min m n) (\a -> KChanged a)
          ]
      ],
      widget Gtk.Separator [
          #margin := 16],
       -- second row (player settings)
      widget Gtk.Label [
          #label := "Players settings: ", 
          #halign := Gtk.AlignCenter,
          #marginBottom := 4, 
          classes ["header"]],
      container Gtk.Box [ -- red player settings
          #orientation := Gtk.OrientationHorizontal,
          #marginTop := 8,
          #spacing := 8
          ] [
          widget Gtk.Label [#label := "Red: ", classes ["settingsLabel", "red"]], 
          container Gtk.Box [#spacing := 8] (if isRedComputer then [
              widget Gtk.Button [#label := "Computer", on #clicked (RedTypeChanged False)],
              widget Gtk.Label [#label := "Difficulty: "],
              minusButton redD GameConstants.minDifficulty (\a -> RedDifficultyChanged a),
              widget Gtk.Label [#label := (GameConstants.difficultyName redD)],
              plusButton redD GameConstants.maxDifficulty (\a -> RedDifficultyChanged a)
          ]
          else [
              widget Gtk.Button [#label := "Player", on #clicked (RedTypeChanged True)]
          ])
      ],
      container Gtk.Box [ -- blue player settings
          #orientation := Gtk.OrientationHorizontal,
          #marginTop := 8,
          #spacing := 8
          ] [
          widget Gtk.Label [#label := "Blue: ", classes ["settingsLabel", "blue"]], 
          container Gtk.Box [#spacing := 8] (if isBlueComputer then [
              widget Gtk.Button [#label := "Computer", on #clicked (BlueTypeChanged False)],
              widget Gtk.Label [#label := "Difficulty: "],
              minusButton blueD GameConstants.minDifficulty (\a -> BlueDifficultyChanged a),
              widget Gtk.Label [#label := (GameConstants.difficultyName blueD)],
              plusButton blueD GameConstants.maxDifficulty (\a -> BlueDifficultyChanged a)
          ]
          else [
              widget Gtk.Button [#label := "Player", on #clicked (BlueTypeChanged True)]
          ])
      ],
      widget Gtk.Separator [
          #margin := 16],
       -- third row (containing start game button)
      container Gtk.Box [
          #orientation := Gtk.OrientationVertical
          ] [
          widget Gtk.Button [#label := "Start game", on #clicked StartGame, #halign := Gtk.AlignCenter]
      ]
  ]
view' (UserVSUserWindow gameState) = 
    gameWindow gameState (isNothing winningPlayer) winningPlayer Nothing
    where
        winningPlayer = Game.whoWins gameState
view' (UserVSComputerWindow gameState@(GameState _ _ currentPlayer) computerColor computerSetting) =
    gameWindow gameState ((isNothing winningPlayer) && (computerColor /= currentPlayer)) winningPlayer Nothing
    where
        winningPlayer = Game.whoWins gameState
view' (ComputerVSComputerWindow gameState redSetting blueSetting speed paused) =
    gameWindow gameState False winningPlayer pauseInformation 
    where
        winningPlayer = Game.whoWins gameState
        pauseInformation = case winningPlayer of
            Nothing -> Just paused
            Just _ -> Nothing




simpleTrans :: State -> Transition State Event
simpleTrans state = Transition state (pure Nothing)

waitAndEvoke :: Maybe Event -> PlaySpeed -> IO (Maybe Event)
waitAndEvoke maybeEvent time = threadDelay time $> maybeEvent

update' :: State -> Event -> Transition State Event
-- closing Window
update' _ Closed = Exit
-- setting field values in Settings Window
update' (SettingsWindow (Game.BoardSetting m n k) redC redD redSeed blueC blueD blueSeed) (MChanged newM) = 
    simpleTrans (SettingsWindow (Game.BoardSetting newM n k) redC redD redSeed blueC blueD blueSeed)
update' (SettingsWindow (Game.BoardSetting m n k) redC redD redSeed blueC blueD blueSeed) (NChanged newN) = 
    simpleTrans (SettingsWindow (Game.BoardSetting m newN k) redC redD redSeed blueC blueD blueSeed) 
update' (SettingsWindow (Game.BoardSetting m n k) redC redD redSeed blueC blueD blueSeed) (KChanged newK) = 
    simpleTrans (SettingsWindow (Game.BoardSetting m n newK) redC redD redSeed blueC blueD blueSeed)
update' (SettingsWindow board redC redD redSeed blueC blueD blueSeed) (RedTypeChanged newRedC) =
    simpleTrans (SettingsWindow board newRedC redD redSeed blueC blueD blueSeed)
update' (SettingsWindow board redC redD redSeed blueC blueD blueSeed) (BlueTypeChanged newBlueC) =
    simpleTrans (SettingsWindow board redC redD redSeed newBlueC blueD blueSeed)
update' (SettingsWindow board redC redD redSeed blueC blueD blueSeed) (RedDifficultyChanged newRedD) =
    simpleTrans (SettingsWindow board redC newRedD redSeed blueC blueD blueSeed)
update' (SettingsWindow board redC redD redSeed blueC blueD blueSeed) (BlueDifficultyChanged newBlueD) =
    simpleTrans (SettingsWindow board redC redD redSeed blueC newBlueD blueSeed)
update' (SettingsWindow board redC redD redSeed blueC blueD blueSeed) (RedSeedChanged newRedSeed) =
    simpleTrans (SettingsWindow board redC redD newRedSeed blueC blueD blueSeed)
update' (SettingsWindow board redC redD redSeed blueC blueD blueSeed) (BlueSeedChanged newBlueSeed) =
    simpleTrans (SettingsWindow board redC redD redSeed blueC blueD newBlueSeed)
-- starting game
update' (SettingsWindow boardSetting True redD redSeed True blueD blueSeed) StartGame = -- both Computers
    let
        redHiperparams = GameConstants.difficultyToHiperparameters redD
        (generatedMove, nextGenerator) = AI.getNextMove (Game.newGame boardSetting) redHiperparams (AI.newGenerator redSeed)
        nextMove = case generatedMove of
            Nothing -> Just (Error "Red cannot choose his first move. This shouldn't happen.")
            Just generatedMoveFromJust -> Just (MovePlayed generatedMoveFromJust)
        startingState = 
            ComputerVSComputerWindow (Game.newGame boardSetting) 
                (AISetting redHiperparams nextGenerator) 
                (AISetting (GameConstants.difficultyToHiperparameters blueD) (AI.newGenerator blueSeed)) 
                defaultSpeed False
    in
    Transition startingState (pure nextMove)
update' (SettingsWindow boardSetting False _ _ False _ _) StartGame = -- both Users
    simpleTrans (UserVSUserWindow (Game.newGame boardSetting))
update' (SettingsWindow boardSetting False _ _ True blueD blueSeed) StartGame = -- red User, blue Computer
    simpleTrans 
        (UserVSComputerWindow (Game.newGame boardSetting) (Game.Blue) 
            (AISetting (GameConstants.difficultyToHiperparameters blueD) (AI.newGenerator blueSeed)))
update' (SettingsWindow boardSetting True redD redSeed False _ _) StartGame = -- red Computer, blue User
    let 
        redHiperparams = GameConstants.difficultyToHiperparameters redD
        (generatedMove, nextGenerator) = AI.getNextMove (Game.newGame boardSetting) redHiperparams (AI.newGenerator redSeed)
        nextMove = case generatedMove of
            Nothing -> Just (Error "Red cannot choose his first move. This shouldn't happen.")
            Just generatedMoveFromJust -> Just (MovePlayed generatedMoveFromJust)
    in
    Transition (UserVSComputerWindow (Game.newGame boardSetting) (Game.Red) (AISetting redHiperparams nextGenerator)) (pure nextMove)
-- quitting game
update' _ EndGame = 
    simpleTrans (defaultSettingsWindow)
-- playing move
update' (UserVSUserWindow gameState) (MovePlayed move) =
    case Game.applyMove gameState move of
        Nothing -> simpleTrans (ErrorWindow "Invalid move played")
        Just nextState -> simpleTrans (UserVSUserWindow nextState)           
update' (UserVSComputerWindow gameState@(GameState _ _ currentPlayer) computerColor computerSetting@(AISetting difficulty generator)) (MovePlayed move)
    | currentPlayer == computerColor = -- computer played move
        case Game.applyMove gameState move of
            Nothing -> simpleTrans (ErrorWindow "Invalid move played")
            Just nextState -> simpleTrans (UserVSComputerWindow nextState computerColor computerSetting)
    | otherwise = -- User played move
        case Game.applyMove gameState move of
            Nothing -> simpleTrans (ErrorWindow "Invalid move played")
            Just nextState -> 
                let 
                    (generatedMove, nextGenerator) = AI.getNextMove nextState difficulty generator
                    nextMove = case (Game.whoWins nextState, generatedMove) of
                        (Nothing, Just generatedMoveFromJust) -> Just (MovePlayed generatedMoveFromJust) -- oponent will play his next move
                        (_ , Nothing) -> Nothing -- there are no possible moves for oponent
                        (Just _, _) -> Nothing -- if move was winning oponent will not have next move
                in
                Transition (UserVSComputerWindow nextState computerColor (AISetting difficulty nextGenerator)) (pure nextMove)
-- playing move in computerVSComputerWindow should work only if it is not paused:
update' (ComputerVSComputerWindow gameState@(GameState _ _ currentPlayer) redSetting@(AISetting redD redG) blueSetting@(AISetting blueD blueG) speed False) (MovePlayed move) =
    case Game.applyMove gameState move of
        Nothing -> simpleTrans (ErrorWindow "Invalid move played")
        Just nextState -> 
            case currentPlayer of
                Game.Red ->
                    let
                        (generatedMove, nextGenerator) = AI.getNextMove nextState redD redG
                        nextMove = case (Game.whoWins nextState, generatedMove) of
                            (Nothing, Just generatedMoveFromJust) -> Just (MovePlayed generatedMoveFromJust) -- oponent will play his next move
                            (_ , Nothing) -> Nothing -- there are no possible moves for oponent
                            (Just _, _) -> Nothing -- if move was winning oponent will not have next move
                        nextWindow = ComputerVSComputerWindow nextState (AISetting redD nextGenerator) (AISetting blueD blueG) speed False
                    in 
                    Transition nextWindow (waitAndEvoke nextMove speed)
                Game.Blue ->
                    let
                        (generatedMove, nextGenerator) = AI.getNextMove nextState blueD blueG
                        nextMove = case (Game.whoWins nextState, generatedMove) of
                            (Nothing, Just generatedMoveFromJust) -> Just (MovePlayed generatedMoveFromJust) -- oponent will play his next move
                            (_ , Nothing) -> Nothing -- there are no possible moves for oponent
                            (Just _, _) -> Nothing -- if move was winning oponent will not have next move
                        nextWindow = ComputerVSComputerWindow nextState (AISetting redD redG) (AISetting blueD nextGenerator) speed False
                    in 
                    Transition nextWindow (waitAndEvoke nextMove speed)
-- but, when we pause ComputerVSComputerWindow there can still be one MovePlayed action waiting in the background - we want to ignore it:
update' cvscw@(ComputerVSComputerWindow _ _ _ _ True) (MovePlayed _) =
    simpleTrans cvscw
-- also when we go back to main menu from ComputerVSComputerWindow there can still be one MovePlayed action waiting in the background - we want to ignore it:
update' sw@(SettingsWindow{..}) (MovePlayed _) =
    simpleTrans sw
-- pausing
update' (ComputerVSComputerWindow gameState redSetting blueSetting speed paused) SimulationPaused = 
    simpleTrans (ComputerVSComputerWindow gameState redSetting blueSetting speed True)
-- resuming
update' (ComputerVSComputerWindow gameState@(GameState _ _ currentPlayer) redSetting@(AISetting redD redG) blueSetting@(AISetting blueD blueG) speed paused) SimulationResumed = 
    case currentPlayer of
        Game.Red ->
            let
                (generatedMove, nextGenerator) = AI.getNextMove gameState redD redG
                nextMove = case (Game.whoWins gameState, generatedMove) of
                    (Nothing, Just generatedMoveFromJust) -> Just (MovePlayed generatedMoveFromJust) -- oponent will play his next move
                    (_ , Nothing) -> Nothing -- there are no possible moves for oponent
                    (Just _, _) -> Nothing -- if move was winning oponent will not have next move
                nextWindow = ComputerVSComputerWindow gameState (AISetting redD nextGenerator) (AISetting blueD blueG) speed False
            in 
            Transition nextWindow (waitAndEvoke nextMove speed)
        Game.Blue ->
            let
                (generatedMove, nextGenerator) = AI.getNextMove gameState blueD blueG
                nextMove = case (Game.whoWins gameState, generatedMove) of
                    (Nothing, Just generatedMoveFromJust) -> Just (MovePlayed generatedMoveFromJust) -- oponent will play his next move
                    (_ , Nothing) -> Nothing -- there are no possible moves for oponent
                    (Just _, _) -> Nothing -- if move was winning oponent will not have next move
                nextWindow = ComputerVSComputerWindow gameState (AISetting redD redG) (AISetting blueD nextGenerator) speed False
            in 
            Transition nextWindow (waitAndEvoke nextMove speed)
--- errors
update' _ (Error text) =
    simpleTrans (ErrorWindow text)
--- actions that shouldn't appear
update' _ _ =
    simpleTrans (ErrorWindow "This shouldn't happen.")


--- Jak doprowadzić do błędu: otworzyć ComputerVSComputer, wrócić do menu i szybko zacząć nową grę - w grze pojawi się jeden ruch, który nie powinien się pojawić - był to oczekujący ruch z symulacji ComputerVSComputer, którego cooldown nie zdążył minąć zanim włączyliśmy drugą grę

initialState' :: State
initialState' = defaultSettingsWindow
