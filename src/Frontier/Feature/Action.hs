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

data Distance = Near | Far
data Consuming = Consume | NoConsume
data Direction = N | NE | E | SE | S | SW | W | NW

data ActionF item object next
    -- Item actions
    = TargetInventoryItem (item -> next)
    | YieldInventoryItem item next
    | ReplaceTargetItem item next
    | DestroyTargetItem next
    | RequireItem Consuming item next
    -- Object actions
    | Me (object -> next)
    | TargetObject Distance (object -> next)
    | ReplaceTargetObject object next
    | DestroyTargetObject next
    | Move Direction object (Bool -> next)
    -- Empty space actions
    | TargetEmptySpace next
    -- Other actions
    | FailAction String next
    | DisableAction next
    deriving (Functor)

type ActionM item object = Free (ActionF item object)

makeFree ''ActionF

-- TODO: DRY
transform :: forall i i' o o' a.
                  (i -> i')
               -> (i' -> i)
               -> (o -> o')
               -> (o' -> o)
               -> ActionM i o a
               -> ActionM i' o' a
transform i2i' i'2i o2o' o'2o = iterM run
    where
        run :: ActionF i o (ActionM i' o' a) -> ActionM i' o' a
        run (TargetInventoryItem next) =
            i'2i `fmap` targetInventoryItem >>= next
        run (YieldInventoryItem item next) =
            yieldInventoryItem (i2i' item) >> next
        run (ReplaceTargetItem item next) =
            replaceTargetItem (i2i' item) >> next
        run (DestroyTargetItem next) =
            destroyTargetItem >> next
        run (RequireItem consuming item next) =
            requireItem consuming (i2i' item) >> next
        run (Me next) =
            o'2o `fmap` me >>= next
        run (TargetObject distance next) =
            o'2o `fmap` targetObject distance >>= next
        run (ReplaceTargetObject obj next) =
            replaceTargetObject (o2o' obj) >> next
        run (DestroyTargetObject next) =
            destroyTargetObject >> next
        run (Move dir obj next) =
            move dir (o2o' obj) >>= next
        run (TargetEmptySpace next) =
            targetEmptySpace >> next
        run (FailAction msg next) =
            failAction msg >> next
        run (DisableAction next) =
            disableAction >> next
