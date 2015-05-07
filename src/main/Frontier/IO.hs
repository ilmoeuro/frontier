{-# LANGUAGE LambdaCase #-}
module Frontier.IO
    (keyboardController
    ,Mode(..)
    ,spritesView
    ,messageView
    ) where

import Data.List
import Control.Monad
import Control.Concurrent.Chan
import Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString.Char8 as Bs8
import Control.Monad.Managed
import MVC hiding (Input, Output)
import MVC.Prelude
import Graphics.Vty hiding (update)
import Frontier.Interaction (Sprite)
import qualified Frontier.Interaction as Model (Input (..))

keyboardController :: Chan Event -> Managed (Controller Model.Input)
keyboardController events
    = producer (bounded 1)
    $ forever
    $ liftIO (readChan events) >>= \case
        (EvKey (KChar c) _)     -> yield . Model.KeyChar $ c
        (EvKey KLeft     _)     -> yield . Model.KeyChar $ 'h'
        (EvKey KDown     _)     -> yield . Model.KeyChar $ 'j'
        (EvKey KUp       _)     -> yield . Model.KeyChar $ 'k'
        (EvKey KRight    _)     -> yield . Model.KeyChar $ 'l'
        (EvKey KEsc      _)     -> yield . Model.KeyChar $ 'q'
        (EvKey _         _)     -> yield . Model.KeyChar $ '\0'
        _                       -> return ()

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
