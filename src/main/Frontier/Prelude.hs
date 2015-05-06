{-# LANGUAGE TupleSections #-}
module Frontier.Prelude
    ((.:)
    ,matches
    ,compose
    ,when'
    ,itemHandles
    ,combineEquals
    ,composeN
    ) where

import Control.Lens
import Data.Monoid

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

matches :: s -> APrism s t a b -> Bool
matches = not .: flip isn't

compose :: [a -> a] -> a -> a
compose = foldr (.) id

when' :: Monoid a => Bool -> a -> a
when' True  x = x
when' False _ = mempty

itemHandles :: String
itemHandles = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['!' .. '/']

combineEquals :: Eq a => [a] -> [(a, Int)]
combineEquals = combine . map (,1) where
    combine ((x,n):(y,1):xs) | x == y   = combine ((x,n+1):xs)
    combine (x:xs)                      = x : combine xs
    combine []                          = []

composeN :: Int -> (a -> a) -> a -> a
composeN = compose .: replicate
