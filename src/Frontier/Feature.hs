{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}
module Frontier.Feature
    (Query(..)
    ,Feature
    ,FeatureRec(..)
    ,featureRec
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

data FeatureRec a = FeatureRec
    {componentFor   :: forall b. Seed b -> a b
    ,initItems      :: [Seed Item]
    ,symbol         :: a Object -> String
    ,command        :: forall c. Char -> (Action a -> c) -> [c]
    ,doTurn         :: forall c. a Object -> (Action a -> c) -> [c]
    ,eq             :: forall b. a b -> a b -> Bool
    ,partialUpdate  :: forall b. a b -> a b -> a b
    }

featureRec :: Feature a -> FeatureRec a
featureRec f = FeatureRec
    {componentFor   =  f .  ComponentFor
    ,initItems      =  f    InitItems
    ,symbol         =  f .  Symbol
    ,command        =  f .: Command
    ,doTurn         =  f .: DoTurn
    ,eq             =  f .: Eq
    ,partialUpdate  =  f .: PartialUpdate
    }
    where
        (.:) = (.).(.)
