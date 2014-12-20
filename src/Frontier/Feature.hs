{-# LANGUAGE GADTs         #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
module Frontier.Feature
    (Query(..)
    ,Feature
    ,(:<+>)()
    ,componentFor
    ,initItems
    ,symbol
    ,command
    ,eq
    ,(<+>)
    ) where

import Frontier.Feature.Action
import Frontier.Feature.Entity
import Frontier.Feature.Qualifier

data Query a result where
    ComponentFor    :: Seed b -> Query a (a b)
    InitItems       :: Query a [Seed Item]
    Symbol          :: a Object -> Query a String
    Command         :: Char
                    -> (forall a'. Feature a' -> ActionM a' () -> b)
                    -> Query a [b]
    Eq              :: a b -> a b -> Query a Bool

type Feature a = forall result. Query a result -> result

data (:<+>) a b c = (:<+>) (a c) (b c)
infixl 5 :<+>

componentFor :: Feature a -> Seed b -> a b
componentFor f = f.ComponentFor

initItems :: Feature a -> [Seed Item]
initItems f = f InitItems

symbol :: Feature a -> a Object -> String
symbol f = f.Symbol

command :: Feature a
        -> Char
        -> (forall a'. Feature a' -> ActionM a' () -> b)
        -> [b]
command f c fn = f (Command c fn)

eq :: Feature a -> a b -> a b -> Bool
eq f a b = f (Eq a b)

(<+>) :: Feature a ->  Feature b -> Feature (a :<+> b)
(<+>) f g (ComponentFor x)              = f (ComponentFor x) :<+> g (ComponentFor x)
(<+>) f g InitItems                     = f InitItems ++ g InitItems
(<+>) f g (Symbol (x :<+> y))           = f (Symbol x) ++ g (Symbol y)
(<+>) f g (Command x y)                 = f (Command x y) ++ g (Command x y)
(<+>) f g (Eq (x :<+> y) (x' :<+> y'))  = f (Eq x x') && g (Eq y y')
infixl 5 <+>
