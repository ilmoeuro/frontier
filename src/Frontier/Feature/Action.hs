{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
module Frontier.Feature.Action
    (ActionF(..)
    ,ActionT
    ,Action
    ,runActionT
    ,transActionT
    ,Consuming(..)
    ,Direction(..)
    -- ActionT actions
    ,shortDescription
    ,targetItem
    ,targetObject
    ,targetEmptySpace
    ,modifyTargetItem
    ,replaceTargetItem
    ,destroyTargetItem
    ,modifyTargetObject
    ,replaceTargetObject
    ,destroyTargetObject
    ,useItem
    ,yieldItem
    ,me
    ,move
    -- Pseudo actions
    ,disabled
    ,requireItem
    ,consumeItem
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Free.TH
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.Trans.Maybe
import Frontier.Feature.Entity
import Frontier.Feature.Qualifier

data Consuming = Consume | NoConsume
    deriving (Eq, Enum, Bounded, Show)
data Direction = N | NE | E | SE | S | SW | W | NW
    deriving (Eq, Enum, Bounded, Show)

data ActionF a next
    = ShortDescription String next
    | TargetItem (a Item -> next)
    | TargetObject (a Object -> next)
    | TargetEmptySpace next
    | ModifyTargetItem (a Item) next
    | ReplaceTargetItem (a Item, Seed Item) next
    | DestroyTargetItem next
    | ModifyTargetObject (a Object) next
    | ReplaceTargetObject (a Object, Seed Object) next
    | DestroyTargetObject next
    | UseItem Consuming (a Item) next
    | YieldItem (a Item, Seed Item) next
    | Me (a Object -> next)
    | Move Direction next
deriving instance Functor (ActionF a)

instance Show (ActionF a next) where
    show = const "<action>"

makeFree ''ActionF

newtype ActionT a m b = ActionT
    { unActionT :: (FreeT (ActionF a) (MaybeT m)) b
    }
deriving instance Monad m           => Functor (ActionT a m)
deriving instance Monad m           => Applicative (ActionT a m)
deriving instance Monad m           => Alternative (ActionT a m)
deriving instance Monad m           => Monad (ActionT a m)
deriving instance Monad m           => MonadPlus (ActionT a m)
deriving instance Monad m           => MonadFree (ActionF a) (ActionT a m)

instance MonadTrans (ActionT a) where
    lift = ActionT . lift . lift

type Action a = forall m. Monad m => ActionT a m ()

runActionT :: forall a m b.
              Monad m
           => (forall t.
                (MonadTrans t
                ,Monad (t m)
                ,MonadPlus (t m)
                ) => ActionF a (t m b) -> t m b)
           -> ActionT a m b
           -> m (Maybe b)
runActionT go = runMaybeT . iterT go . unActionT

transActionT :: forall a b m d.
                Monad m
             => (forall c. a c -> b c)
             -> (forall c. b c -> a c)
             -> ActionT a m d
             -> ActionT b m d
transActionT f g = ActionT . transFreeT go . unActionT where
    go :: ActionF a next -> ActionF b next
    go (ShortDescription s n)           = ShortDescription s n
    go (TargetItem n)                   = TargetItem (n . g)
    go (TargetObject n)                 = TargetObject (n . g)
    go (TargetEmptySpace n)             = TargetEmptySpace n
    go (ModifyTargetItem i n)           = ModifyTargetItem (f i) n
    go (ReplaceTargetItem (i,s) n)      = ReplaceTargetItem (f i,s) n
    go (DestroyTargetItem n)            = DestroyTargetItem n
    go (ModifyTargetObject o n)         = ModifyTargetObject (f o) n
    go (ReplaceTargetObject (o,s) n)    = ReplaceTargetObject (f o,s) n
    go (DestroyTargetObject n)          = DestroyTargetObject n
    go (UseItem c i n)                  = UseItem c (f i) n
    go (YieldItem (i,s) n)              = YieldItem (f i,s) n
    go (Me n)                           = Me (n . g)
    go (Move d n)                       = Move d n

disabled :: Monad m => ActionT a m b
disabled = mzero

requireItem :: a Item -> Action a
requireItem = useItem NoConsume

consumeItem :: a Item -> Action a
consumeItem = useItem Consume
