{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE RankNTypes     #-}
module Frontier.Model.Core.Feature
    (Env(..)
    ,Feature(..)
    ,Item()
    ,Object()
    ,Witness(..)
    ,Tag(..)
    ,Action
    ) where

import Control.Lens

data Item
data Object

data Witness a where
    Item                :: Witness Item
    Object              :: Witness Object

data Tag b where
    PlayerCharacterTag  :: Tag Object
    HammerTag           :: Tag Item
    AxeTag              :: Tag Item
    WorldItemTag        :: Tag Item -> Tag Object
    OpaqueTag           :: Tag b

type Action w = w -> w

data Env w e = Env
    {create             :: forall b. Witness b -> Tag b -> (e b -> e b) -> Action w
    ,withAll            :: forall b. Witness b -> ([e b] -> Action w) -> Action w
    ,modify             :: forall b. Witness b -> (e b -> e b) -> e b -> Action w
    ,destroy            :: forall b. Witness b -> e b -> Action w
    ,is                 :: forall b. e b -> e b -> Bool
    ,_position          :: Lens' (e Object) (Int, Int)
    ,_symbol            :: Lens' (e Object) Char
    }

data Feature w = Feature
    {init               :: Action w
    ,command            :: String
                        -> Action w
    ,step               :: Action w
    }
