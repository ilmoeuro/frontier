{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Frontier.Model.Static
    (Direction(..)
    ,Object(..)
    ,Item(..)
    ,World(..)
    ,_objects
    ,_items
    ,_playerCharacter
    ,defaultWorld
    ) where

import Control.Lens.TH
import Data.Map (Map)
import qualified Data.Map as Map
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import System.Random

_ROWS :: Int
_ROWS = 23

_COLS :: Int
_COLS = 79

data Direction = N | E | S | W deriving (Read, Show)

data Object
    = Wall
    | Tree
    | PlayerCharacter
    deriving (Read, Show, Eq)

data Item
    = Saw
    | Axe
    | Hammer
    | Lumber
    deriving (Read, Show, Ord, Eq)

data World = World
    { objects :: Map (Int, Int) Object
    , items :: MultiSet Item
    , playerCharacter :: ((Int, Int), Object)
    }
    deriving (Read, Show)

makeLensesFor
    [("objects"         ,"_objects")
    ,("items"           ,"_items")
    ,("playerCharacter" ,"_playerCharacter")
    ]
    ''World

defaultWorld :: World
defaultWorld = World
    { objects = initialObjects
    , items = MultiSet.fromList [Saw, Hammer, Axe]
    , playerCharacter = ((1,1), PlayerCharacter)
    }
    where
        initialObjects = Map.fromList $ randomTrees ++ walls
        walls =  map (,Wall)
                    ([(x,0)         | x <- [0.._COLS]]
                  ++ [(x,_ROWS-1)   | x <- [0.._COLS]]
                  ++ [(0,y)         | y <- [1.._ROWS-2]]
                  ++ [(_COLS,y)     | y <- [1.._ROWS-2]])
        randomTrees = map (,Tree) . take 100 $ zip
            (randomRs (1,_COLS-2) (mkStdGen 0))
            (randomRs (1,_ROWS-2) (mkStdGen 1))