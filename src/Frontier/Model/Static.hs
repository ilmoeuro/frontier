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
    ,symbol
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
    { objects = randomTrees
    , items = []
    , playerCharacter = ((0,0), PlayerCharacter)
    }
    where
        randomTrees = fromList . map (,Tree) . take 50 $ zip
            (randomRs (0,80) (mkStdGen 0))
            (randomRs (0,23) (mkStdGen 1))


symbol :: Object -> Char
symbol Wall = '#'
symbol Tree = '^'
symbol PlayerCharacter = '@'
