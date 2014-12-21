{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Frontier.Feature.Base
    (baseFeature
    ) where

import Frontier.Feature
import Frontier.Feature.Entity

baseFeature :: (forall b. a b -> a b -> Bool)
            -> (forall b. a b)
            -> Feature a
            -> Feature a
baseFeature equ blank f = \case
    (ComponentFor Blank)        -> blank

    (Eq a b)
        |  a `equ` blank
        || b `equ` blank        -> True
        | otherwise             -> a `equ` b

    (PartialUpdate a b)
        | b `equ` blank         -> a
        | otherwise             -> b

    others                      -> f others
