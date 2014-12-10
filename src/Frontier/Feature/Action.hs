{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontier.Feature.Action
    (ActionF(..)
    ,ActionM
    -- ActionM actions
    ,targetInventoryItem
    ,yieldInventoryItem
    ,replaceTargetItem
    ,destroyTargetItem
    ,consumeItem
    ,requireItem
    ,targetNearObject
    ,replaceTargetObject
    ,destroyTargetObject
    ,targetEmptySpace
    ,failAction
    ,disableAction
    -- Meta functions
    ,transformTypes
    ) where

import Control.Monad.Free 
import Control.Monad.Free.TH

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
    ConsumeItem
        :: item
        -> next
        -> ActionF item object next
    RequireItem
        :: item
        -> next
        -> ActionF item object next
        
    -- Object actions
    TargetNearObject
        :: (object -> next)
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

transformTypes :: (i -> i') 
               -> (o -> o')
               -> ActionM i o a
               -> ActionM i' o' a
transformTypes = undefined
