{-# LANGUAGE RankNTypes #-}
module Frontier.Feature.GenericMethods
    (genericEq
    ,genericPartialUpdate
    ) where

genericEq :: Eq (a c) => (forall c'. a c') -> a c -> a c -> Bool
genericEq blank a b |  a == blank
                    || b == blank = True
                    |  otherwise  = a == b

genericPartialUpdate :: Eq (a c) => (forall c'. a c') -> a c -> a c -> a c
genericPartialUpdate blank a b | a == blank     = a
                               | otherwise      = b
