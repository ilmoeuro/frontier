{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Frontier.Feature.Action
    (ActionF(..)
    ,ActionM
    ,Distance(..)
    ,Consuming(..)
    ,Direction(..)
    -- ActionM actions
    ,shortDescription
    ,targetInventoryItem
    ,yieldInventoryItem
    ,replaceTargetItem
    ,destroyTargetItem
    ,requireItem
    ,me
    ,targetObject
    ,replaceTargetObject
    ,destroyTargetObject
    ,move
    ,targetEmptySpace
    -- Pseudo actions
    ,disabled
    ) where

import Control.Monad
import Control.Monad.Free.TH
import Control.Monad.Trans.Free
import Frontier.Feature.Qualifier

data Distance = Near | Far
    deriving (Eq, Enum, Bounded, Show)
data Consuming = Consume | NoConsume
    deriving (Eq, Enum, Bounded, Show)
data Direction = N | NE | E | SE | S | SW | W | NW
    deriving (Eq, Enum, Bounded, Show)

data ActionF a next
    -- Meta information
    = ShortDescription String next
    -- Item actions
    | TargetInventoryItem (a Item -> next)
    | YieldInventoryItem (a Item) next
    | ReplaceTargetItem (a Item) next
    | DestroyTargetItem next
    | RequireItem Consuming (a Item) next
    -- Object actions
    | Me (a Object -> next)
    | TargetObject Distance (a Object -> next)
    | ReplaceTargetObject (a Object) next
    | DestroyTargetObject next
    | Move Direction (a Object) (Bool -> next)
    -- Empty space actions
    | TargetEmptySpace next
    deriving (Functor)

instance Show (ActionF a next) where
    show = const "<action>"

type ActionM a = FreeT (ActionF a) Maybe

makeFree ''ActionF

disabled :: ActionM a b
disabled = mzero
