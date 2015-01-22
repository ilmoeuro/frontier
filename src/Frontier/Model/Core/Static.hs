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
    ,_objects
    ,_items
    ,_lastUid
    ,mkMeta
    ,seed
    ,mkWorld
    ) where

import Control.Lens hiding (Action, act)
import Frontier.Model.Core.Feature
import qualified Frontier.Model.Core.Features.Base as Base
import qualified Frontier.Model.Core.Features.Building as Building

type Id = Int

data Components b = Components
    {base               :: Base.Component b
    ,building           :: Building.Component b
    }

data Meta b where
    ObjectMeta          :: (Int, Int) -> Char -> Meta Object
    ItemMeta            :: Meta Item

data Entity b = Entity
    {components         :: Components b
    ,meta               :: Meta b
    ,uid                :: Id
    }

data World = World
    {objects            :: [Entity Object]
    ,items              :: [Entity Item]
    ,lastUid            :: Id
    }

makeLensesFor
    [("base"                ,"_base")
    ,("building"            ,"_building")
    ]
    ''Components

makeLensesFor
    [("components"          ,"_components")
    ,("meta"                ,"_meta")
    ]
    ''Entity

makeLensesFor
    [("objects"             ,"_objects")
    ,("items"               ,"_items")
    ,("lastUid"             ,"_lastUid")
    ]
    ''World

__position :: Lens' (Meta Object) (Int, Int)
__position
    = lens  (\(ObjectMeta p _)      -> p)
            (\(ObjectMeta _ c) p    -> ObjectMeta p c)

__symbol :: Lens' (Meta Object) Char
__symbol
    = lens  (\(ObjectMeta _ c)      -> c)
            (\(ObjectMeta p _) c    -> ObjectMeta p c)

mkMeta :: Witness b -> Meta b
mkMeta Object   = ObjectMeta (0,0) '?'
mkMeta Item     = ItemMeta

seed :: Witness b -> Id -> Seed b -> Entity b
seed wit uid s =
    Entity
        {components = Components
            {base = Base.seed s
            ,building = Building.seed s
            }
        ,meta = mkMeta wit
        ,uid = uid
        }

mkWorld :: World
mkWorld = World
    {objects    = []
    ,items      = []
    ,lastUid    = 0
    }
