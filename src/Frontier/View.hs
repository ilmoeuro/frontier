{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}
module Frontier.View
    (vtyOutput
    ) where

import Control.Monad
import Control.Monad.Managed
import Data.Default
import Data.List
import Data.Map (assocs)
import qualified Frontier.Model.Dynamic.Interface as Dy
import Frontier.Model.Static (World (..), objects, playerCharacter)
import qualified Frontier.Model.Static as St
import Graphics.Vty
import MVC hiding (Input)
import MVC.Prelude

symbol :: St.Object -> Char
symbol St.Wall              = '#'
symbol St.Tree              = '^'
symbol St.PlayerCharacter   = '@'

printCombined :: Show a => (Int, a) -> String
printCombined (1, a)    = show a
printCombined (n, a)    = show a ++ " (x" ++ show n ++ ")"

combine :: Eq a => [a] -> [(Int, a)]
combine = go . map (1,) where
    go ((n,a):(1,b):xs) | a == b    = go ((n+1, a) : xs)
    go (x : xs)                     = x : go xs
    go []                           = []

vtyOutput :: (Picture -> IO ()) -> Managed (View Dy.Output)
vtyOutput update = consumer . forever $ await >>= \case
    (Dy.Display (World{playerCharacter,objects,items})) ->
        liftIO
        . update
        . picForLayers
        $ layers
      where
        ((px,py),pobj) = playerCharacter
        pcLayer = translate px py
                . char def
                . symbol
                $ pobj
        objLayers = map
                  $ \((x,y),obj) ->
                    translate x y
                    . char def
                    . symbol
                    $ obj
        itemsLayer = translate 0 23
                   . string def
                   . intercalate ", "
                   . map printCombined
                   . combine
                   . sort
                   $ items
        layers = pcLayer
               : itemsLayer
               : objLayers (assocs objects)
