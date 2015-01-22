{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE RankNTypes     #-}
module Frontier.Model.Core.Feature
    (Env(..)
    ,Feature(..)
    ,Item()
    ,Object()
    ,Witness(..)
    ,Seed(..)
    ,Action
    ) where

import Control.Lens hiding (Action)

data Item
data Object

data Witness a where
    Item                :: Witness Item
    Object              :: Witness Object

data Seed b where
    PlayerCharacter     :: Seed Object
    Opaque              :: Seed b

type Action w = w -> w

data Env w e = Env
    {create             :: forall b. Witness b -> Seed b -> (e b -> e b) -> Action w
    ,withAll            :: forall b. Witness b -> ([e b] -> Action w) -> Action w
    ,modify             :: forall b. Witness b -> (e b -> e b) -> e b -> Action w
    ,destroy            :: forall b. Witness b -> e b -> Action w
    ,is                 :: forall b. e b -> e b -> Bool
    ,_position          :: Lens' (e Object) (Int, Int)
    ,_symbol            :: Lens' (e Object) Char
    }

data Feature a w e = Feature
    {init               :: Env w e
                        -> (forall b. ALens' (e b) (a b))
                        -> Action w
    ,command            :: String
                        -> Env w e
                        -> (forall b. ALens' (e b) (a b))
                        -> Action w
    ,step               :: Env w e
                        -> (forall b. ALens' (e b) (a b))
                        -> Action w
    }
