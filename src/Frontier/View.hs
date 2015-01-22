{-# LANGUAGE LambdaCase #-}
module Frontier.View
    (spritesView
    ,messageView
    ) where

import Control.Monad
import Control.Monad.Managed
import Data.Default
import Frontier.Model (Sprite)
import Graphics.Vty hiding (update)
import MVC hiding (Input)
import MVC.Prelude

spritesView :: (Picture -> IO ()) -> Managed (View [Sprite])
spritesView update
    = consumer
    . forever
    $ await >>= \sprites ->
       liftIO
       . update
       . picForLayers
       . map toLayer
       $ sprites
  where
    toLayer ((x, y), c) = translate x y . char def $ c

messageView :: (Picture -> IO ()) -> Managed (View [String])
messageView update
    = consumer
    . forever
    $ await >>= \strings ->
       liftIO
       . update
       . picForImage
       . foldr ((<->) . string def) emptyImage
       $ strings
