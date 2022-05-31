-- {-# LANGUAGE OverloadedLabels  #-}
-- {-# LANGUAGE OverloadedLists   #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards   #-}
-- --{-# LANGUAGE FieldSelectors #-}

module UI2(
--     State (..), 
--     Event (..), 
--     view', 
--     update', 
--     initialState'
) where

-- import Game
-- import GameConstants
-- import AI

-- import qualified GI.Gtk as Gtk
-- import GI.Gtk.Declarative
-- import GI.Gtk.Declarative.App.Simple
-- import GI.Gtk.Declarative.Container.Grid

-- import qualified Data.Sequence as Seq
-- import Data.Text (Text, pack)
-- import Data.Vector (Vector, fromList, (++))
-- import Data.Foldable (toList)
-- import Data.Int
-- import Data.Maybe
-- import System.Random


-- data PlayerSettings 
--     = UserSettings 
--     | ComputerSettings {
--         difficulty :: GameConstants.Difficulty,
--         seed :: Int
--     }

-- data Settings 
--     = Settings {
--         redPlayerSettings :: PlayerSettings,
--         bluePlayerSettings :: PlayerSettings,
--         boardSettings :: Game.BoardSetting
--     }

-- data PlayerInGame
--     = User
--     | Computer {
--         hiperparameters :: AI.Hiperparameters,
--         generator :: AI.Generator
--     }

-- data State
--     = SettingsWindow {
--         settings :: Settings
--     }
--     | GameWindow {
--         gameState :: Game.GameState,
--         redPlayer :: PlayerInGame,
--         bluePlayer :: PlayerInGame,
--         currentPlayer :: Game.Player
--     }
--     | ErrorWindow {
--         message :: Text
--     }

-- data Event
--     = Closed
--     | Error Text
--     | BackToMenu
--     | StartGame
--     | PlayMove {
--         movePlayed :: Game.Move
--     }
--     | AwaitMove
--     | ChangeSettings {
--         newSettings :: Settings
--     }


-- defaultComputerPlayerSettings :: PlayerSettings
-- defaultComputerPlayerSettings = ComputerSettings {
--     difficulty = GameConstants.defaultDifficulty,
--     seed = 137
-- }

-- defaultSettings :: Settings
-- defaultSettings = Settings {
--     redPlayerSettings = UserSettings,
--     bluePlayerSettings = UserSettings,
--     boardSettings = GameConstants.defaultBoardSetting
-- }

-- initialState' :: State
-- initialState' = SettingsWindow defaultSettings


-- newPlayerInGame :: PlayerSettings -> PlayerInGame
-- newPlayerInGame settings@(ComputerSettings{..}) = Computer {
--     hiperparameters = (GameConstants.difficultyToHiperparameters (difficulty settings)),
--     generator = (AI.newGenerator (seed settings))
-- }
-- newPlayerInGame UserSettings = User


-- simpleTrans :: State -> Transition State Event
-- simpleTrans state = Transition state (pure Nothing)

-- update' :: State -> Event -> Transition State Event
-- -- closing Window
-- update' _ Closed = Exit
-- -- settings change
-- update' (SettingsWindow _) (ChangeSettings newSettings) = 
--     simpleTrans (SettingsWindow newSettings)
-- -- end game
-- update' _ BackToMenu = 
--     simpleTrans initialState'
-- -- start game
-- update' (SettingsWindow settings) StartGame = 
--     Transition gameWindow (pure (Just AwaitMove))
--     where 
--         gameWindow = GameWindow {
--             gameState = Game.newGame (boardSettings settings),
--             redPlayer = newPlayerInGame (redPlayerSettings settings),
--             bluePlayer = newPlayerInGame (bluePlayerSettings settings),
--             currentPlayer = Game.startingPlayer
--         }
-- -- play move
-- update' oldGameWindow@(GameWindow{..}) (PlayMove move) =
--     case maybeNextGameState of
--         Nothing -> simpleTrans (ErrorWindow "Incorrect move played")
--         Just nextGameState -> 
--             let 
--                 nextGameWindow = GameWindow {
--                     gameState = nextGameState,
--                     redPlayer = (redPlayer oldGameWindow),
--                     bluePlayer = (bluePlayer oldGameWindow),
--                     currentPlayer = Game.oponent (currentPlayer oldGameWindow)
--                 }
--             in
--             Transition nextGameWindow (pure (Just AwaitMove))
--     where
--         maybeNextGameState = Game.applyMove (gameState oldGameWindow) move
        
