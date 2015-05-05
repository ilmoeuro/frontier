{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
module Frontier.Model.Core.Feature
    (LevelSource
    ,Env(..)
    ,Feature(..)
    ,Item()
    ,Object()
    ,Witness(..)
    ,Tag(..)
    ,Action
    ) where

import Prelude hiding (init)
import Control.Lens
import Data.Monoid

type LevelSource = String
type InitParam = Int

data Item
data Object

data Witness a where
    Item                :: Witness Item
    Object              :: Witness Object

data Tag b where
    PlayerCharacterTag  :: Tag Object
    WallTag             :: Tag Object
    HammerTag           :: Tag Item
    AxeTag              :: Tag Item
    LumberTag           :: Tag Item
    WorldItemTag        :: Tag Item -> Tag Object
    OpaqueTag           :: Tag b

deriving instance Eq (Tag b)
deriving instance Show (Tag b)

type Action w = w -> w

data Env w e = Env
    -- Perform actions
    { create            :: forall b. Witness b
                        -> Tag b 
                        -> (e b -> e b) 
                        -> Action w
    , modify            :: forall b. Witness b 
                        -> (e b -> e b) 
                        -> e b 
                        -> Action w
    , destroy           :: forall b. Witness b
                        -> e b 
                        -> Action w
    , message           :: ([String] -> [String])
                        -> Action w
    -- Supply arguments to continuations
    , withAll           :: forall b. Witness b
                        -> ([e b] -> Action w)
                        -> Action w
    , withInitParam     :: (InitParam -> Action w)
                        -> Action w
    -- Query & modify entities
    , is                :: forall b. e b
                        -> e b
                        -> Bool
    , _position         :: Lens' (e Object) (Int, Int)
    , _symbol           :: Lens' (e Object) Char
    , _zIndex           :: Lens' (e Object) Int
    , _tag              :: forall b. Lens' (e b) (Tag b)
    }

data Feature w = Feature
    { init              :: Action w
    , command           :: String
                        -> Action w
    , step              :: Action w
    , loadLevel         :: LevelSource
                        -> Action w
    }

instance Monoid (Feature w) where
    mempty = Feature
        { init          = id
        , command       = const id
        , step          = id
        , loadLevel     = const id
        }
    mappend f1 f2 = Feature
        { init          = init f1 . init f2
        , command       = \x -> command f1 x . command f2 x
        , step          = step f1 . step f2
        , loadLevel     = \x -> loadLevel f1 x . loadLevel f2 x
        }
