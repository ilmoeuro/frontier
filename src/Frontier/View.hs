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
import Frontier.Model.Static (PlayerCharacter (..), World (..), objects,
                              playerCharacter)
import qualified Frontier.Model.Static as St
import Graphics.Vty
import MVC hiding (Input)
import MVC.Prelude

symbol :: St.Object -> Char
symbol St.Wall              = '#'
symbol St.Tree              = '^'
symbol (St.Box _)           = '='

showWithCount :: Show a => (a, Int) -> String
showWithCount (a, 1)    = show a
showWithCount (a, n)    = show a ++ " (x" ++ show n ++ ")"

layers :: World -> String -> [Image]
layers world message =
    pcLayer
       : statusLayer
       : msgLayer
       : objLayers (assocs objects)
    where
        World{playerCharacter,objects,items} = world
        ((px,py), PlayerCharacter{energy}) = playerCharacter
        pcLayer = translate px py
                . char def
                $ '@'
        objLayers = map
                  $ \((x,y),obj) ->
                    translate x y
                    . char def
                    . symbol
                    $ obj
        msgLayer = translate 0 22
                 . string def
                 $ message
        statusLayer = translate 0 23
                    . string def
                    $ "Energy: "
                    ++ show (energy `div` 10)
                    ++ " Inv: "
                    ++ (intercalate ", "
                       . map showWithCount
                       . toOccurList
                       $ items)

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
