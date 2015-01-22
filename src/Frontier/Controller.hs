{-# LANGUAGE LambdaCase #-}
module Frontier.Controller
    (keyboardController
    ) where

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Managed
import qualified Frontier.Model.Core as Model (Input (..))
import Graphics.Vty
import MVC hiding (Input)
import MVC.Prelude

-- TODO: Separate
keyboardController :: Chan Event -> Managed (Controller Model.Input)
keyboardController events
    = producer Single $ do
        yield Model.Init
        forever $ liftIO (readChan events) >>= \case
            EvKey (KChar c) _ -> do
                                    yield . Model.Command $ c
                                    yield Model.Step
            _                 -> return ()
