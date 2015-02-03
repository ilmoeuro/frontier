{-# LANGUAGE GADTs           #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontier.Model.Core.Static
    (Id
    ,Components(..)
    ,Meta(..)
    ,Entity(..)
    ,World(..)
    ,_base
    ,_building
    ,_components
    ,_meta
    ,__position
    ,__symbol
    ,__zIndex
    ,_entityTag
    ,_objects
    ,_items
    ,_lastUid
    ,_initParam
    ,_messages
    ,_dirtyTiles
    ,mkMeta
    ,seed
    ,mkWorld
    ) where

import Control.Lens
import Frontier.Model.Core.Feature
import qualified Frontier.Model.Core.Features.Base as Base
import qualified Frontier.Model.Core.Features.Building as Building

type Id = Int

data Components b = Components
    {base               :: Base.Component b
    ,building           :: Building.Component b
    }

data Meta b where
    ObjectMeta          :: (Int, Int) -> Char -> Int -> Meta Object
    ItemMeta            :: Meta Item

data Entity b = Entity
    {components         :: Components b
    ,meta               :: Meta b
    ,entityTag          :: Tag b
    ,uid                :: Id
    }

data World = World
    {objects            :: [Entity Object]
    ,items              :: [Entity Item]
    ,lastUid            :: Id
    ,initParam          :: Int
    ,messages           :: [String]
    ,dirtyTiles         :: [(Int, Int)]
    }

makeLensesFor
    [("base"                ,"_base")
    ,("building"            ,"_building")
    ]
    ''Components

makeLensesFor
    [("components"          ,"_components")
    ,("meta"                ,"_meta")
    ,("entityTag"           ,"_entityTag")
    ]
    ''Entity

makeLensesFor
    [("objects"             ,"_objects")
    ,("items"               ,"_items")
    ,("lastUid"             ,"_lastUid")
    ,("initParam"           ,"_initParam")
    ,("messages"            ,"_messages")
    ,("dirtyTiles"          ,"_dirtyTiles")
    ]
    ''World

__position :: Lens' (Meta Object) (Int, Int)
__position
    = lens  (\(ObjectMeta p _ _)    -> p)
            (\(ObjectMeta _ c z) p  -> ObjectMeta p c z)

__symbol :: Lens' (Meta Object) Char
__symbol
    = lens  (\(ObjectMeta _ c _)    -> c)
            (\(ObjectMeta p _ z) c  -> ObjectMeta p c z)

__zIndex :: Lens' (Meta Object) Int
__zIndex
    = lens  (\(ObjectMeta _ _ z)    -> z)
            (\(ObjectMeta p c _) z  -> ObjectMeta p c z)

mkMeta :: Witness b -> Meta b
mkMeta Object   = ObjectMeta (0,0) '?' 0
mkMeta Item     = ItemMeta

seed :: Witness b -> Id -> Tag b -> Entity b
seed wit uid s =
    Entity
        {components = Components
            {base = Base.fromTag s
            ,building = Building.fromTag s
            }
        ,meta = mkMeta wit
        ,entityTag = s
        ,uid = uid
        }

mkWorld :: World
mkWorld = World
    {objects    = []
    ,items      = []
    ,lastUid    = 0
    ,initParam  = 0
    ,messages   = []
    ,dirtyTiles = []
    }
