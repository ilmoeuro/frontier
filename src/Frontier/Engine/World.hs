{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
module Frontier.Engine.World
    (WorldState(..)
    ,_objects
    ,_playerCharacter
    ,_inventory
    ,_targetObject
    ,_targetItem
    ,_targetEmptySpace
    ,neighbors
    ) where

import Control.Lens.TH
import Data.Maybe
import Frontier.Feature.Action
import Frontier.Feature.Qualifier

data WorldState a = WorldState
    {objects            :: [((Int, Int), a Object)]
    ,playerCharacter    :: ((Int, Int), a Object)
    ,inventory          :: [a Item]
    ,targetObject       :: ((Int, Int), a Object)
    ,targetItem         :: a Item
    ,targetEmptySpace   :: (Int,Int)
    }

makeLensesFor
    [("objects"             ,"_objects")
    ,("playerCharacter"     ,"_playerCharacter")
    ,("inventory"           ,"_inventory")
    ,("targetObject"        ,"_targetObject")
    ,("targetItem"          ,"_targetItem")
    ,("targetEmptySpace"    ,"_targetEmptySpace")
    ]
    ''WorldState

neighbors :: WorldState a -> [(Direction, a Object)]
neighbors WorldState{playerCharacter,objects} = mapMaybe go points where
    go (p, dir) = (dir,) `fmap` lookup p objects
    ((x, y), _) = playerCharacter
    points =
        [((x,   y-1),   N)
        ,((x+1, y-1),   NE)
        ,((x+1, y),     E)
        ,((x+1, y+1),   SE)
        ,((x,   y+1),   S)
        ,((x-1, y+1),   SW)
        ,((x-1, y),     W)
        ,((x-1, y-1),   NW)
        ]
