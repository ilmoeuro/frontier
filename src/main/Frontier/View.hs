{-# LANGUAGE LambdaCase #-}
module Frontier.View
    (spritesView
    ,messageView
    ) where

import Control.Monad
import Control.Monad.Managed
import Data.ByteString.UTF8 (fromString)
import Data.List
import Frontier.Model (Sprite)
import Graphics.Vty hiding (update)
import MVC hiding (Input, Output)
import MVC.Prelude

spritesView :: Output -> Managed (View (String, [Sprite]))
spritesView output
    = consumer
    $ do
        hideCursor output
        forever $ do
            (_, sprites) <- await
            forM_ sprites $ \((x, y), c) -> liftIO $ do
                setCursorPos output x y
                outputByteBuffer output (fromString [c])

messageView :: Output -> Managed (View [String])
messageView output
    = consumer
    . forever
    $ await >>= \strings -> liftIO $ do
        setCursorPos output 0 0
        outputByteBuffer output
            ( fromString
            . intercalate "\n"
            $ strings)
