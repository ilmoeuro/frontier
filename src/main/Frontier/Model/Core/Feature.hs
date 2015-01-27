{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
module Frontier.Model.Core.Feature
    (Env(..)
    ,Feature(..)
    ,Item()
    ,Object()
    ,Witness(..)
    ,Tag(..)
    ,Size(..)
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
    LumberTag           :: Tag Item
    WorldItemTag        :: Tag Item -> Tag Object
    OpaqueTag           :: Tag b

data Size
    = Small
    | Large
    deriving (Show, Eq)

deriving instance Eq (Tag b)
deriving instance Show (Tag b)

type Action w = w -> w

data Env w e = Env
    {create             :: forall b. Witness b -> Tag b -> (e b -> e b) -> Action w
    ,withAll            :: forall b. Witness b -> ([e b] -> Action w) -> Action w
    ,modify             :: forall b. Witness b -> (e b -> e b) -> e b -> Action w
    ,destroy            :: forall b. Witness b -> e b -> Action w
    ,is                 :: forall b. e b -> e b -> Bool
    ,withInitParam      :: (Int -> Action w) -> Action w
    ,message            :: ([String] -> [String]) -> Action w
    ,_position          :: Lens' (e Object) (Int, Int)
    ,_symbol            :: Lens' (e Object) Char
    ,_tag               :: forall b. Lens' (e b) (Tag b)
    ,_size              :: Lens' (e Object) Size
    }

data Feature w = Feature
    {init               :: Action w
    ,command            :: String
                        -> Action w
    ,step               :: Action w
    }
