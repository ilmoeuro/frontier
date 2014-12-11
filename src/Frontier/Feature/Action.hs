{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontier.Feature.Action
    (ActionF(..)
    ,ActionM
    ,Distance(..)
    ,Consuming(..)
    -- ActionM actions
    ,targetInventoryItem
    ,yieldInventoryItem
    ,replaceTargetItem
    ,destroyTargetItem
    ,requireItem
    ,targetObject
    ,replaceTargetObject
    ,destroyTargetObject
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

data ActionF item object next where
    -- Item actions
    TargetInventoryItem 
        :: (item -> next)
        -> ActionF item object next
    YieldInventoryItem
        :: item
        -> next
        -> ActionF item object next
    ReplaceTargetItem
        :: item
        -> next
        -> ActionF item object next
    DestroyTargetItem
        :: next
        -> ActionF item object next
    RequireItem
        :: Consuming
        -> item
        -> next
        -> ActionF item object next
        
    -- Object actions
    TargetObject
        :: Distance
        -> (object -> next)
        -> ActionF item object next
    ReplaceTargetObject
        :: object
        -> next
        -> ActionF item object next
    DestroyTargetObject
        :: next
        -> ActionF item object next
        
    -- Empty space actions
    TargetEmptySpace
        :: next
        -> ActionF item object next
        
    -- Other actions
    FailAction
        :: String
        -> next
        -> ActionF item object next
    DisableAction
        :: next
        -> ActionF item object next
        
    deriving (Functor)
    
type ActionM item object = Free (ActionF item object)

makeFree ''ActionF

-- TODO: DRY
transform :: forall i o i' o' a.
                  (i -> i')
               -> (i' -> i) 
               -> (o -> o')
               -> (o' -> o)
               -> ActionM i o a
               -> ActionM i' o' a
transform i2i' i'2i o2o' o'2o = iterM run
    where
        run :: ActionF i o (ActionM i' o' a) -> ActionM i' o' a
        run (TargetInventoryItem next) = do
            item <- i'2i `fmap` targetInventoryItem
            next item
        run (YieldInventoryItem item next) = do
            yieldInventoryItem $ i2i' item
            next
        run (ReplaceTargetItem item next) = do
            replaceTargetItem $ i2i' item
            next
        run (DestroyTargetItem next) = do
            destroyTargetItem
            next
        run (RequireItem consuming item next) = do
            requireItem consuming (i2i' item)
            next
        run (TargetObject distance next) = do
            obj <- o'2o `fmap` targetObject distance
            next obj
        run (ReplaceTargetObject obj next) = do
            replaceTargetObject (o2o' obj)
            next
        run (DestroyTargetObject next) = do
            destroyTargetObject
            next
        run (TargetEmptySpace next) = do
            targetEmptySpace
            next
        run (FailAction msg next) = do
            failAction msg
            next
        run (DisableAction next) = do
            disableAction
            next