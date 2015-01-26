module Frontier.Prelude
    ((.:)
    ,matches
    ,compose
    ,when'
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
