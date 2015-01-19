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
import Data.MultiSet (toOccurList)
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
symbol (St.Box _)           = '='

showWithCount :: Show a => (a, Int) -> String
showWithCount (a, 1)    = show a
showWithCount (a, n)    = show a ++ " (x" ++ show n ++ ")"

vtyOutput :: (Picture -> IO ()) -> Managed (View Dy.Output)
vtyOutput update = consumer . forever $ await >>= \case
    (Dy.Display world) ->
        liftIO
        . update
        . picForLayers
        $ layers world ""
    (Dy.Message msg world) ->
        liftIO
        . update
        . picForLayers
        $ layers world msg
  where
    layers world message =
        let
            World{playerCharacter,objects,items} = world
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
            msgLayer = translate 0 22
                     . string def
                     $ message
            itemsLayer = translate 0 23
                       . string def
                       . ("Inv.: " ++)
                       . intercalate ", "
                       . map showWithCount
                       . toOccurList
                       $ items
        in pcLayer
           : itemsLayer
           : msgLayer
           : objLayers (assocs objects)
