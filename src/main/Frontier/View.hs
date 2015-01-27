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

spritesView :: (Picture -> IO ()) -> Managed (View (String, [Sprite]))
spritesView update
    = consumer
    . forever
    $ await >>= \(msg, sprites) ->
       liftIO
       . update
       . picForLayers
       $ map toLayer sprites
         ++ [msgLayer msg]
  where
    toLayer ((x, y), c) = translate x y . char def $ c
    msgLayer = translate 0 23 . string def

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
