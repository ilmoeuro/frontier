{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}
module Frontier.Feature
    (Query(..)
    ,Feature
    ,componentFor
    ,initItems
    ,symbol
    ,command
    ,doTurn
    ,eq
    ,partialUpdate
    ) where

import Frontier.Feature.Action
import Frontier.Feature.Entity
import Frontier.Feature.Qualifier

data Query a result where
    ComponentFor    :: Seed b -> Query a (a b)
    InitItems       :: Query a [Seed Item]
    Symbol          :: a Object -> Query a String
    Command         :: Char
                    -> (Action a -> c)
                    -> Query a [c]
    DoTurn          :: a Object
                    -> (Action a -> c)
                    -> Query a [c]
    Eq              :: a b -> a b -> Query a Bool
    PartialUpdate   :: a b -> a b -> Query a (a b)

type Feature a = forall result. Query a result -> result

componentFor :: Feature a -> Seed b -> a b
componentFor f = f.ComponentFor

initItems :: Feature a -> [Seed Item]
initItems f = f InitItems

symbol :: Feature a -> a Object -> String
symbol f = f.Symbol

command :: Feature a
        -> Char
        -> (ActionM a () -> c)
        -> [c]
command f c fn = f (Command c fn)

doTurn :: Feature a
       -> a Object
       -> (ActionM a () -> c)
       -> [c]
doTurn f o fn = f (DoTurn o fn)

eq :: Feature a -> a b -> a b -> Bool
eq f a b = f (Eq a b)

partialUpdate :: Feature a -> a b -> a b -> a b
partialUpdate f a b = f (PartialUpdate a b)
