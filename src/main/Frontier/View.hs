{-# LANGUAGE LambdaCase #-}
module Frontier.View
    (Mode(..)
    ,spritesView
    ,messageView
    ) where

import Control.Monad
import Control.Monad.Managed
import qualified Data.ByteString.Char8 as Bs8
import Data.ByteString.UTF8 (fromString)
import Data.List
import Frontier.Model (Sprite)
import Graphics.Vty hiding (update)
import MVC hiding (Input, Output)
import MVC.Prelude

data Mode
    = ClearScreen
    | NoClearScreen
    deriving (Eq)

clearScreen :: Output -> IO ()
clearScreen output = do
    (w,h) <- displayBounds output
    forM_ [0..h] $ \y -> do
        setCursorPos output 0 y
        outputByteBuffer output
            . Bs8.replicate w
            $ ' '

spritesView :: Mode -> Output -> Managed (View (String, [Sprite]))
spritesView mode output
    = consumer $ do
        hideCursor output
        forever $ do
            (msg, sprites) <- await
            when (mode == ClearScreen) $ liftIO (clearScreen output)
            forM_ sprites $ \((x, y), c) -> liftIO $ do
                setCursorPos output x y
                outputByteBuffer output (fromString [c])
            showMessage $ replicate 80 ' '
            showMessage msg
  where
    showMessage msg = do
        setCursorPos output 0 23
        liftIO
            . outputByteBuffer output
            . fromString
            $ msg

messageView :: Output -> Managed (View [String])
messageView output
    = consumer
    . forever
    $ await >>= \strings -> liftIO $ do
        clearScreen output
        setCursorPos output 0 0
        outputByteBuffer output
            ( fromString
            . intercalate "\n"
            $ strings)
