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
    -- Meta functions
    ,transform
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

-- TODO: DRY
transform :: forall a a' c.
                  (forall b. a b -> a' b)
               -> (forall b. a' b -> a b)
               -> ActionM a c
               -> ActionM a' c
transform a2a' a'2a = iterM run
    where
        run :: ActionF a (ActionM a' c) -> ActionM a' c
        run (TargetInventoryItem next) =
            a'2a `fmap` targetInventoryItem >>= next
        run (YieldInventoryItem item next) =
            yieldInventoryItem (a2a' item) >> next
        run (ReplaceTargetItem item next) =
            replaceTargetItem (a2a' item) >> next
        run (DestroyTargetItem next) =
            destroyTargetItem >> next
        run (RequireItem consuming item next) =
            requireItem consuming (a2a' item) >> next
        run (Me next) =
            a'2a `fmap` me >>= next
        run (TargetObject distance next) =
            a'2a `fmap` targetObject distance >>= next
        run (ReplaceTargetObject obj next) =
            replaceTargetObject (a2a' obj) >> next
        run (DestroyTargetObject next) =
            destroyTargetObject >> next
        run (Move dir obj next) =
            move dir (a2a' obj) >>= next
        run (TargetEmptySpace next) =
            targetEmptySpace >> next
        run (FailAction msg next) =
            failAction msg >> next
        run (DisableAction next) =
            disableAction >> next
