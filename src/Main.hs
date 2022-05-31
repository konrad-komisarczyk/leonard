module Main where

import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import Control.Monad
import UI


main :: IO ()
main = void $ run App {view = UI.view', update = UI.update', inputs = [], initialState = UI.initialState'}