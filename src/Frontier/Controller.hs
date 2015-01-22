{-# LANGUAGE LambdaCase #-}
module Frontier.Controller
    (keyboardController
    ) where

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Managed
import qualified Frontier.Model as Model (Input (..))
import Graphics.Vty
import MVC hiding (Input)
import MVC.Prelude

keyboardController :: Chan Event -> Managed (Controller Model.Input)
keyboardController events
    = producer Single
    $ forever
    $ liftIO (readChan events) >>= \case
        (EvKey (KChar c) _)     -> yield . Model.KeyChar $ c
        _                       -> return ()
