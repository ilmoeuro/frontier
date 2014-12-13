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
    ,failAction
    ,disableAction
    ) where

import Control.Monad.Free
import Control.Monad.Free.TH
import Frontier.Feature.Qualifier

data Distance = Near | Far
data Consuming = Consume | NoConsume
data Direction = N | NE | E | SE | S | SW | W | NW

data ActionF a next
    -- Item actions
    = TargetInventoryItem (a Item -> next)
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
    -- Other actions
    | FailAction String next
    | DisableAction next
    deriving (Functor)

type ActionM a = Free (ActionF a)

makeFree ''ActionF
