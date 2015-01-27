{-# LANGUAGE TupleSections #-}
module Frontier.Prelude
    ((.:)
    ,matches
    ,compose
    ,when'
    ,itemHandles
    ,combineEquals
    ) where

import Control.Lens

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

matches :: s -> APrism s t a b -> Bool
matches = not .: flip isn't

compose :: [a -> a] -> a -> a
compose = foldr (.) id

when' :: Bool -> (w -> w) -> w -> w
when' True  x = x
when' False _ = id

itemHandles :: String
itemHandles = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['!' .. '/']

combineEquals :: Eq a => [a] -> [(a, Int)]
combineEquals = combine . map (,1) where
    combine ((x,n):(y,1):xs) | x == y   = combine ((x,n+1):xs)
    combine (x:xs)                      = x : combine xs
    combine []                          = []
