{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import Control.Monad
import UI
import           Data.ByteString                ( ByteString )
import           Control.Concurrent.Async       ( async )
import Data.Text


styles :: ByteString
styles = mconcat
  [".red {color: red;}"
  , ".blue {color: #00BFFF;}"
  , ".winInformation {font-size: xx-large; font-weight: bold;}"
  , ".header {font-size: large; font-weight: bold;}"
  , ".settingsLabel {font-weight: bold;}"

  ]


main :: IO ()
main = do
  void $ Gtk.init Nothing

  -- Set up screen and CSS provider
  screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
  p      <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData p styles
  Gtk.styleContextAddProviderForScreen
    screen
    p
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

  -- Start main loop
  void . async $ do
    void $ runLoop app
    Gtk.mainQuit
  Gtk.main
 where
  app = App { view = view', update = update', inputs = [], initialState = UI.initialState' }