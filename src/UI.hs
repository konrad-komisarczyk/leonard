{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : UI
Description : UI rendering functions

Provides types and functions needed to render λeonard app with gi-gtk-declarative-app-simple framework.
-}
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
import Data.Text (Text, pack, append)
import Data.Vector (Vector, fromList, (++))
import Data.Foldable (toList)
import Data.Int
import Data.Maybe
import System.Random
import Control.Concurrent (threadDelay)


-- | Default additional delay between moves in Computer VS Computer game.
--   Set to 0.2 seconds
defaultSpeed :: Int
defaultSpeed = 2


minSpeed :: Int
minSpeed = 0


maxSpeed :: Int
maxSpeed = 100


-- | Type containing parameters and random generator for the MCTS AI player function. 
data AISetting = 
    AISetting {
        -- | Hyperparameters for the MCTS AI player function. 
        --   Set based on the chosen AI difficulty. Constant during the game.
        hiperparameters :: AI.Hiperparameters, 
        -- | Random number generator state. Changes after every AI move.
        generator :: AI.Generator
    }


-- | Application states. 
--   Each state corresponds to one type of window application can render and values that have to be stored in that state.
data State 
    -- | Main menu window
    = SettingsWindow {
        boardSetting :: Game.BoardSetting, 
        isRedComputer :: Bool, 
        redDifficulty :: GameConstants.Difficulty, 
        redSeed :: Int,
        isBlueComputer :: Bool, 
        blueDifficulty :: GameConstants.Difficulty,
        blueSeed :: Int,
        speed :: Int}
    -- | User VS User game window.
    --   Shows: game board, buttons with available moves, winner information when a player wins, "Back to menu" button.
    --   Users can play moves clicking corresponding buttons. After every move game board is immediately refreshed.
    | UserVSUserWindow {
        gameState :: Game.GameState}
    -- | User VS User game window. 
    --   Shows: game board, buttons with available moves when its Users turn, winner information when a player wins, "Back to menu" button.
    --   User can play moves clicking corresponding buttons. 
    --   After every move game board is immediately refreshed, then immediately Computer plays his move and board is refreshed again.
    | UserVSComputerWindow {
        gameState :: Game.GameState, 
        computerColor :: Game.Player, 
        computerSetting :: AISetting} 
    -- | Computer VS Computer game window. Also called Simulation. 
    --   Shows: game board, "Pause"/"Resume" button, winner information when a player wins, "Back to menu" button.
    --   Computer players play moves alternately. There is added a fixed delay 'speed' between the moves. Immediately after every move the board is refreshed.
    --   Simulation can be paused and resumed any time.
    | ComputerVSComputerWindow {
        gameState :: Game.GameState, 
        red :: AISetting, 
        blue :: AISetting, 
        speed :: Int, 
        paused :: Bool}
    -- | Error window containing single error message.
    --   State should not be reachable in the application.
    | ErrorWindow {
        message :: Text}


-- | Possible events. Each event depending on current state changes state and then does some IO action and finally maybe invokes other events.
data Event 
    = Closed 
    | Error Text
    -- | Settings modification events: (Evoked every time value is changed in the SettingsWindow) 
    --   Each *Changed event is parametrized by the new value setting should take
    | MChanged Int 
    | NChanged Int 
    | KChanged Int
    | RedTypeChanged Bool 
    | BlueTypeChanged Bool 
    | RedDifficultyChanged GameConstants.Difficulty
    | BlueDifficultyChanged GameConstants.Difficulty
    | RedSeedChanged Int
    | BlueSeedChanged Int
    | SpeedChanged Int
    | StartGame 
    -- | Game events: 
    --   Should appear only in one of the game windows
    | EndGame 
    | MovePlayed Game.Move
    | SimulationPaused 
    | SimulationResumed


-- | Default values of SettingsWindow State
defaultSettingsWindow :: State
defaultSettingsWindow = SettingsWindow GameConstants.defaultBoardSetting False GameConstants.defaultDifficulty 137 False GameConstants.defaultDifficulty 213 defaultSpeed


-- | Window containing whole application
mainWrapper 
    :: Widget Event -- ^ Content of the window. Single widget, usually a Box containing all the children.
    -> AppView Gtk.Window Event
mainWrapper child = bin Gtk.Window
  [ #title := "λeonard", 
  on #deleteEvent (const (True, Closed)), 
  #resizable := False
  ]
  $ child


-- | Button with "+" label evoking Int parametrized event modyfing a variable. 
--   Inactive when value of a given variable would exceed given bound.
plusButton 
    :: Int -- ^ 'currVal' - Current value of the variable button modifies
    -> Int -- ^ Max value of the variable button modifies
    -> (Int -> Event) -- ^ 'event' - Function for a given 'val' Int returning event modyfing the variable to a 'val' value.
    -> BoxChild Event -- ^ Resulting Button widget. Clicking button evokes 'event (currVal + 1)'
plusButton currVal maxVal event = 
    widget Gtk.Button [
        #label := "+", 
        on #clicked (event (currVal + 1)), 
        #sensitive := (currVal < maxVal)
        ]

-- | Button with "-" label evoking Int parametrized event modyfing a variable. 
--   Inactive when value of a given variable would exceed given bound.
minusButton 
    :: Int -- ^ 'currVal' - Current value of the variable button modifies
    -> Int -- ^ Min value of the variable button modifies
    -> (Int -> Event) -- ^ 'event' - Function for a given 'val' Int returning event modyfing the variable to a 'val' value.
    -> BoxChild Event -- ^ Resulting Button widget. Clicking button evokes 'event (currVal - 1)'
minusButton currVal minVal event = 
    widget Gtk.Button [
        #label := "-", 
        on #clicked (event (currVal - 1)), 
        #sensitive := (currVal > minVal)
        ]


-- | Map board field value to an appropiate image representing token or empty field
boardElemToImage :: Maybe Player -> Widget Event
boardElemToImage Nothing = widget Gtk.Image [#file := "img/empty.png"]
boardElemToImage (Just Red) = widget Gtk.Image [#file := "img/red.png"]
boardElemToImage (Just Blue) = widget Gtk.Image [#file := "img/blue.png"]


-- | Map a board field to a Grid child containing image representing appropiate token or empty field
boardElemToGridChild 
    :: Int -- ^ number of rows
    -> Int -- ^ index of the field in the BoardState Sequence
    -> Maybe Player -- ^ value of the field in the BoardState Sequence
    -> GridChild Event -- ^ resulting Grid child
boardElemToGridChild m index elem = GridChild {
    properties = defaultGridChildProperties {
        width = 1, height = 1, 
        leftAttach = fromIntegral (div index m), 
        topAttach = (fromIntegral (m - (mod index m))) + 1
        },
    child = (boardElemToImage elem)
}


-- | Map board state to Vector of Grid children containing images representing appropiate tokens or empty fields
boardToGridChildren 
    :: Int -- ^ number of rows
    -> BoardState -> Vector (GridChild Event)
boardToGridChildren m board = fromList (toList (Seq.mapWithIndex (boardElemToGridChild m) board))


-- | Map a single move to a button. Clicking that button will evoke 'MovePlayed' event.
moveToButton :: Player -> Move -> Widget Event
moveToButton Red move = bin Gtk.Button [on #clicked (MovePlayed move), #margin := 4] (widget Gtk.Image [#file := "img/redArrow.png"])
moveToButton Blue move = bin Gtk.Button [on #clicked (MovePlayed move), #margin := 4] (widget Gtk.Image [#file := "img/blueArrow.png"])


-- | Map a single move to Grid element containing move button
moveToGridChild :: Player -> Move -> GridChild Event
moveToGridChild player move = GridChild {
    properties = defaultGridChildProperties {
        width = 1, height = 1,
        leftAttach = ((fromIntegral move) :: Int32), 
        topAttach = 0
        },
    child = (moveToButton player move)
}


-- | Map moves to Grid elements containing move buttons
movesToGridChildren :: Player -> [Move] -> Vector (GridChild Event)
movesToGridChildren player moves = fromList (map (moveToGridChild player) moves)


-- | Map game state to Grid widget
gameGrid :: GameState 
    -> Bool -- ^ Whether to show move buttons at the top of the grid. Buttons should be shown only if it's Users turn. 
    -> BoxChild Event
gameGrid gameState@(GameState bs@(BoardSetting m n _) board player) showMoves = container Gtk.Grid [
    #rowSpacing := 0,
    #columnSpacing := 0
    ] gridElements where
        gridElements
            | showMoves = (boardToGridChildren m board) Data.Vector.++ (movesToGridChildren player (Game.availableMoves gameState))
            | otherwise = boardToGridChildren m board


-- | Content of win information box in game window.
--   For Nothing returns empty vector
--   For Just Player returns vector with single label saying, that given player wins
winInformation :: Maybe Player -> Vector (BoxChild Event)
winInformation Nothing = []
winInformation (Just Blue) = [widget Gtk.Label [#label := "Blue player wins!", #marginTop := 8, classes ["winInformation", "blue"]]]
winInformation (Just Red) = [widget Gtk.Label [#label := "Red player wins!", #marginTop := 8, classes ["winInformation", "red"]]]


-- | Content of pause button box in game window.
--   For Nothing returns empty vector
--   For Just True returns vector with single "Resume" button
--   For Just False returns vector with single "Pause" button
pauseButton :: Maybe Bool -> Vector (BoxChild Event)
pauseButton Nothing = []
pauseButton (Just True) = [widget Gtk.Button [#label := "Resume", on #clicked SimulationResumed]]
pauseButton (Just False) = [widget Gtk.Button [#label := "Pause", on #clicked SimulationPaused]]


-- | Returns a render of game window. 
gameWindow :: GameState 
    -> Bool -- ^ Whether to show buttons with possible moves, that allow user to play that moves (should be true only if user has turn)
    -> Maybe Player -- ^ Information about current winner. When Just player info about winner is displayed.
    -> Maybe Bool -- ^ When 'Just paused' (in AI VS AI games) shows pause/resume button. When Nothing does not show that button.
    -> AppView Gtk.Window Event
gameWindow gameState showMoves winningPlayer maybePaused = mainWrapper
  $ container Gtk.Box [
        #orientation := Gtk.OrientationVertical, 
        #halign := Gtk.AlignCenter] [
        container Gtk.Box [
            #orientation := Gtk.OrientationVertical, 
            #halign := Gtk.AlignCenter
            ] (winInformation winningPlayer),
        container Gtk.Box [
            #orientation := Gtk.OrientationVertical, 
            #halign := Gtk.AlignCenter
            ] (pauseButton maybePaused),
        container Gtk.Box [
            #orientation := Gtk.OrientationHorizontal
            ] [
            container Gtk.Box [ -- board container
                #orientation := Gtk.OrientationVertical,
                #margin := 12
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

-- | Returns a render of a given state
view' :: State -> AppView Gtk.Window Event
-- Error window
view' (ErrorWindow message) = 
    mainWrapper $ widget Gtk.Label [#label := message]
-- Settings window
view' (SettingsWindow boardSetting@(BoardSetting m n k) isRedComputer redD redSeed isBlueComputer blueD blueSeed speed) = 
    mainWrapper $ container Gtk.Box [
        #orientation := Gtk.OrientationVertical,
        #margin := 10
        ] [
        -- banner
        widget Gtk.Image [
            #halign := Gtk.AlignCenter, 
            #file := "img/banner.png"
        ],
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
                widget Gtk.Label [#label := (Data.Text.pack (show m)), #widthChars := 2, #xalign := 0.5],
                plusButton m GameConstants.maxRows (\a -> MChanged a)
            ],
            container Gtk.Box [ -- columns settings
                #marginTop := 8,
                #spacing := 8
            ] [
                widget Gtk.Label [#label := "Number of columns: ", classes ["settingsLabel"]],
                minusButton n k (\a -> NChanged a),
                widget Gtk.Label [#label := (Data.Text.pack (show n)), #widthChars := 2, #xalign := 0.5],
                plusButton n GameConstants.maxColumns (\a -> NChanged a)
            ],
            container Gtk.Box [ -- winning line settings
                #marginTop := 8,
                #spacing := 8
            ] [
                widget Gtk.Label [#label := "Length of line to win: ", classes ["settingsLabel"]],
                minusButton k GameConstants.minLine (\a -> KChanged a),
                widget Gtk.Label [#label := (Data.Text.pack (show k)), #widthChars := 2, #xalign := 0.5],
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
                widget Gtk.Separator [],
                widget Gtk.Label [#label := "Difficulty:"],
                minusButton redD GameConstants.minDifficulty (\a -> RedDifficultyChanged a),
                widget Gtk.Label [#label := (GameConstants.difficultyName redD), #widthChars := GameConstants.maxDifficultyNameLength, #xalign := 0.5],
                plusButton redD GameConstants.maxDifficulty (\a -> RedDifficultyChanged a),
                widget Gtk.Separator [],
                widget Gtk.Label [#label := "Seed:"],
                widget Gtk.Label [#label := (Data.Text.pack (show redSeed))],
                widget Gtk.Button [#label := "🎲", on #clicked (RedSeedChanged (fst (System.Random.uniformR (10, 9999) (System.Random.mkStdGen redSeed))))]
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
                widget Gtk.Separator [],
                widget Gtk.Label [#label := "Difficulty:"],
                minusButton blueD GameConstants.minDifficulty (\a -> BlueDifficultyChanged a),
                widget Gtk.Label [#label := (GameConstants.difficultyName blueD), #widthChars := GameConstants.maxDifficultyNameLength, #xalign := 0.5],
                plusButton blueD GameConstants.maxDifficulty (\a -> BlueDifficultyChanged a),
                widget Gtk.Separator [],
                widget Gtk.Label [#label := "Seed:"],
                widget Gtk.Label [#label := (Data.Text.pack (show blueSeed)), #widthChars := 4],
                widget Gtk.Button [#label := "🎲", on #clicked (BlueSeedChanged (fst (System.Random.uniformR (10, 9999) (System.Random.mkStdGen blueSeed))))]
            ]
            else [
                widget Gtk.Button [#label := "Player", on #clicked (BlueTypeChanged True)]
            ])
        ],
        container Gtk.Box [ -- speed settings
            #orientation := Gtk.OrientationHorizontal,
            #marginTop := 8,
            #spacing := 8
        ] (if isBlueComputer && isRedComputer then [
            widget Gtk.Label [#label := "Additional delay between moves (not from computing AI's move): "],
            minusButton speed minSpeed (\a -> SpeedChanged a),
            widget Gtk.Label [#label := ((Data.Text.pack (show ((fromIntegral speed) / 10))) `Data.Text.append` "s")],
            plusButton speed maxSpeed (\a -> SpeedChanged a)
            ] else []),
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


-- | Just transition to a given state not doing any IO action or producing any more events.
simpleTrans :: State -> Transition State Event
simpleTrans state = Transition state (pure Nothing)


-- | Wait given time and return given MaybeEvent.
waitAndEvoke :: Maybe Event -> Int -> IO (Maybe Event)
waitAndEvoke maybeEvent time = threadDelay (time * 100 * 1000) >> pure maybeEvent


-- | For a given state and an event returns Transition to a next state.
--   Transition contains next state and does some IO action that may result in a next event that will be evoken.
update' :: State -> Event -> Transition State Event
-- closing Window
update' _ Closed = Exit
-- setting field values in Settings Window
update' (SettingsWindow (Game.BoardSetting m n k) redC redD redSeed blueC blueD blueSeed speed) (MChanged newM) = 
    simpleTrans (SettingsWindow (Game.BoardSetting newM n k) redC redD redSeed blueC blueD blueSeed speed)
update' (SettingsWindow (Game.BoardSetting m n k) redC redD redSeed blueC blueD blueSeed speed) (NChanged newN) = 
    simpleTrans (SettingsWindow (Game.BoardSetting m newN k) redC redD redSeed blueC blueD blueSeed speed) 
update' (SettingsWindow (Game.BoardSetting m n k) redC redD redSeed blueC blueD blueSeed speed) (KChanged newK) = 
    simpleTrans (SettingsWindow (Game.BoardSetting m n newK) redC redD redSeed blueC blueD blueSeed speed)
update' (SettingsWindow board redC redD redSeed blueC blueD blueSeed speed) (RedTypeChanged newRedC) =
    simpleTrans (SettingsWindow board newRedC redD redSeed blueC blueD blueSeed speed)
update' (SettingsWindow board redC redD redSeed blueC blueD blueSeed speed) (BlueTypeChanged newBlueC) =
    simpleTrans (SettingsWindow board redC redD redSeed newBlueC blueD blueSeed speed)
update' (SettingsWindow board redC redD redSeed blueC blueD blueSeed speed) (RedDifficultyChanged newRedD) =
    simpleTrans (SettingsWindow board redC newRedD redSeed blueC blueD blueSeed speed)
update' (SettingsWindow board redC redD redSeed blueC blueD blueSeed speed) (BlueDifficultyChanged newBlueD) =
    simpleTrans (SettingsWindow board redC redD redSeed blueC newBlueD blueSeed speed)
update' (SettingsWindow board redC redD redSeed blueC blueD blueSeed speed) (RedSeedChanged newRedSeed) =
    simpleTrans (SettingsWindow board redC redD newRedSeed blueC blueD blueSeed speed)
update' (SettingsWindow board redC redD redSeed blueC blueD blueSeed speed) (BlueSeedChanged newBlueSeed) =
    simpleTrans (SettingsWindow board redC redD redSeed blueC blueD newBlueSeed speed)
update' (SettingsWindow board redC redD redSeed blueC blueD blueSeed speed) (SpeedChanged newSpeed) =
    simpleTrans (SettingsWindow board redC redD redSeed blueC blueD blueSeed newSpeed)
-- starting game
update' (SettingsWindow boardSetting True redD redSeed True blueD blueSeed speed) StartGame = -- both Computers
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
                speed False
    in
    Transition startingState (pure nextMove)
update' (SettingsWindow boardSetting False _ _ False _ _ _) StartGame = -- both Users
    simpleTrans (UserVSUserWindow (Game.newGame boardSetting))
update' (SettingsWindow boardSetting False _ _ True blueD blueSeed _) StartGame = -- red User, blue Computer
    simpleTrans 
        (UserVSComputerWindow (Game.newGame boardSetting) (Game.Blue) 
            (AISetting (GameConstants.difficultyToHiperparameters blueD) (AI.newGenerator blueSeed)))
update' (SettingsWindow boardSetting True redD redSeed False _ _ _) StartGame = -- red Computer, blue User
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


-- | Initial state of the application. 
--   Application starts with the SettingsWindow.
initialState' :: State
initialState' = defaultSettingsWindow
