{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes         #-}
module Frontier.Feature
    (Query(..)
    ,Feature
    ,Feature'
    ,comp
    ) where

import Control.Arrow
import Frontier.Feature.Action (ActionM)
import Frontier.Feature.Qualifier

data Query a result where
    InitItems               :: Query a [a Item]
    Symbol                  :: a Object -> Query a Char
    InitPlayerCharacter     :: Query a (a Object)
    Command                 :: Char -> Query a (a (Action ()))
    Eq                      :: (forall b. a b -> a b) -> Query a Bool
    -- TODO: Polymorphic
    Run                     :: a (Action ()) -> Query a (ActionM a ())

type Feature' a = forall result. Query a result -> result
type Feature a b c d = (Feature' a, b -> c -> d)

comp :: (b -> c, b1 -> c1)
        -> (b' -> c')
        -> ((b, b') -> (c, c'), (b'1 -> c'1) -> (b1, b'1) -> (c1, c'1))
comp (ftr,k) ftr' = (ftr *** ftr', (k ***))
