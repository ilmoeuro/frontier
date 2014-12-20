{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}
module Frontier.Feature
    (Query(..)
    ,Feature
    ,(:<+>)()
    ,(<+>)
    ) where

import Frontier.Feature.Action
import Frontier.Feature.Entity
import Frontier.Feature.Qualifier

data Query a result where
    ComponentFor    :: Entity b -> Query a (a b)
    InitItems       :: Query a [Entity Item]
    Symbol          :: a Object -> Query a String
    Command         :: Char -> (forall a'. ActionM a' () -> b) -> Query a [b]
    Eq              :: a b -> a b -> Query a Bool

type Feature a = forall result. Query a result -> result

data (:<+>) a b c = (:<+>) (a c) (b c)

(<+>) :: Feature a ->  Feature b -> Feature (a :<+> b)
(<+>) f g (ComponentFor x)              = f (ComponentFor x) :<+> g (ComponentFor x)
(<+>) f g InitItems                     = f InitItems ++ g InitItems
(<+>) f g (Symbol (x :<+> y))           = f (Symbol x) ++ g (Symbol y)
(<+>) f g (Command x y)                 = f (Command x y) ++ g (Command x y)
(<+>) f g (Eq (x :<+> y) (x' :<+> y'))  = f (Eq x x') && g (Eq y y')
