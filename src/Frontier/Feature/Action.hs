{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Frontier.Feature.Action
    (ActionF(..)
    ,ActionM
    ,Action
    ,Consuming(..)
    ,Direction(..)
    ,Outcome(..)
    ,Targeting(..)
    -- ActionM actions
    ,shortDescription
    ,target
    ,yieldItem
    ,me
    ,move
    -- Pseudo actions
    ,disabled
    ,retain
    ,modify
    ,replaceWith
    ,destroy
    ,requireItem
    ,consumeItem
    ) where

import Control.Monad
import Control.Monad.Free.TH
import Control.Monad.Trans.Free
import Frontier.Feature.Entity
import Frontier.Feature.Qualifier

data Consuming = Consume | NoConsume
    deriving (Eq, Enum, Bounded, Show)
data Direction = N | NE | E | SE | S | SW | W | NW
    deriving (Eq, Enum, Bounded, Show)
data Outcome a b
    = Retain
    | Modify (a b)
    | ReplaceWith (a b, Seed b)
    | Destroy
    deriving (Show)
data Targeting a
    = InventoryItem (a Item -> ActionM a (Outcome a Item))
    | NearObject (a Object -> ActionM a (Outcome a Object))
    | EmptySpace (ActionM a (Outcome a Object))

data ActionF a next
    -- Meta information
    = ShortDescription String next
    -- Item actions
    | Target (Targeting a) next
    | UseItem Consuming (a Item) next
    | YieldItem (a Item) next
    -- Object actions
    | Me (a Object -> next)
    | Move Direction (a Object) next
    deriving (Functor)

instance Show (ActionF a next) where
    show = const "<action>"

type ActionM a = FreeT (ActionF a) Maybe
type Action a = ActionM a ()

makeFree ''ActionF

disabled :: ActionM a b
disabled = mzero

retain :: ActionM a (Outcome a b)
retain = return Retain

modify :: a b -> ActionM a (Outcome a b)
modify = return . Modify

replaceWith :: (a b, Seed b) -> ActionM a (Outcome a b)
replaceWith = return . ReplaceWith

destroy :: ActionM a (Outcome a b)
destroy = return Destroy

requireItem :: a Item -> Action a
requireItem = useItem NoConsume

consumeItem :: a Item -> Action a
consumeItem = useItem Consume
