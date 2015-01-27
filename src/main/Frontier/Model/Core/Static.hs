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
    ,_entityTag
    ,_objects
    ,_items
    ,_lastUid
    ,_initParam
    ,_messages
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
    ObjectMeta          :: (Int, Int) -> Char -> Meta Object
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
    }
