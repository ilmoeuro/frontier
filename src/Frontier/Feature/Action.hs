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
    ,requireItem
    ,yieldItem
    ,me
    ,move
    -- Pseudo actions
    ,disabled
    ,retain
    ,replaceWith
    ,destroy
    ) where

import Control.Monad
import Control.Monad.Free.TH
import Control.Monad.Trans.Free
import Frontier.Feature.Qualifier

data Consuming = Consume | NoConsume
    deriving (Eq, Enum, Bounded, Show)
data Direction = N | NE | E | SE | S | SW | W | NW
    deriving (Eq, Enum, Bounded, Show)
data Outcome a b = Retain | ReplaceWith (a b) | Destroy
    deriving (Show)
data Targeting a
    = InventoryItem (a Item -> ActionM a (Outcome a Item))
    | NearObject (a Object -> ActionM a (Outcome a Object))
    | FarObject (a Object -> ActionM a (Outcome a Object))
    | EmptySpace (ActionM a (Outcome a Object))

data ActionF a next
    -- Meta information
    = ShortDescription String next
    -- Item actions
    | Target (Targeting a) next
    | RequireItem Consuming (a Item) next
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

replaceWith :: a b -> ActionM a (Outcome a b)
replaceWith = return . ReplaceWith

destroy :: ActionM a (Outcome a b)
destroy = return Destroy
