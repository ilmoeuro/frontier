{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}
module Frontier.Feature
    (Feature(..)
    ) where

import Frontier.Feature.Action
import Frontier.Feature.Entity
import Frontier.Feature.Qualifier

data Feature a = Feature
    {componentFor   :: forall b. Seed b -> a b
    ,initItems      :: [Seed Item]
    ,symbol         :: a Object -> String
    ,command        :: forall c. Char -> (Action a -> c) -> [c]
    ,doTurn         :: forall c. a Object -> (Action a -> c) -> [c]
    ,eq             :: forall b. a b -> a b -> Bool
    ,partialUpdate  :: forall b. a b -> a b -> a b
    }
