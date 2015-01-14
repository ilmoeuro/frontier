{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TemplateHaskell   #-}
module Frontier.Data
    (Direction(..)
    ,Object(..)
    ,Item(..)
    ,World(..)
    ,_objects
    ,_items
    ,_playerCharacter
    ) where

import Control.Lens.TH
import Data.Map (Map)

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
