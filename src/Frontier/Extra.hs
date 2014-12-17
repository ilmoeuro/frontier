{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Frontier.Extra
    (headOr
    ,singleOr
    ) where

import Data.Foldable
import Data.Traversable

data NEL a
    = One a
    | And a (NEL a)
    deriving (Show, Eq, Functor, Foldable, Traversable)

headOr :: [a] -> a -> a
headOr (x:_) _ = x
headOr _     x = x

singleOr :: [a] -> a -> a
singleOr [x] _ = x
singleOr _   x = x
