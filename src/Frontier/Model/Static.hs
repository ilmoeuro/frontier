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
import Data.Map (Map, fromList)
import System.Random

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
    deriving (Read, Show, Eq)

data World = World
    { objects :: Map (Int, Int) Object
    , items :: [Item]
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
    , items = [Axe, Saw, Hammer]
    , playerCharacter = ((1,1), PlayerCharacter)
    }
    where
        initialObjects = fromList $ randomTrees ++ walls
        walls =  map (,Wall)
                    ([(x,0)     | x <- [0..79]]
                  ++ [(x,23)    | x <- [0..79]]
                  ++ [(0,y)     | y <- [1..22]]
                  ++ [(79,y)    | y <- [1..22]])
        randomTrees = map (,Tree) . take 100 $ zip
            (randomRs (1,79) (mkStdGen 0))
            (randomRs (1,22) (mkStdGen 1))
