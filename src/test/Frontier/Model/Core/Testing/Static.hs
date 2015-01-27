{-# LANGUAGE GADTs           #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontier.Model.Core.Testing.Static
    (Id
    ,Meta(..)
    ,Entity(..)
    ,World(..)
    ,_component
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

type Id = Int

data Meta b where
    ObjectMeta          :: (Int, Int) -> Char -> Meta Object
    ItemMeta            :: Meta Item

data Entity c b = Entity
    {component          :: c b
    ,meta               :: Meta b
    ,entityTag          :: Tag b
    ,uid                :: Id
    }

data World c = World
    {objects            :: [Entity c Object]
    ,items              :: [Entity c Item]
    ,lastUid            :: Id
    ,initParam          :: Int
    ,messages           :: [String]
    }

makeLensesFor
    [("component"           ,"_component")
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

seed :: Witness b -> Id -> (Tag b -> c b) -> Tag b -> Entity c b
seed wit uid fromTag s =
    Entity
        {component = fromTag s
        ,meta = mkMeta wit
        ,entityTag = s
        ,uid = uid
        }

mkWorld :: World c
mkWorld = World
    {objects    = []
    ,items      = []
    ,lastUid    = 0
    ,initParam  = 0
    ,messages   = []
    }
