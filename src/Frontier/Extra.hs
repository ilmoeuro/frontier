module Frontier.Extra
    (headOr
    ,singleOr
    ) where

headOr :: [a] -> a -> a
headOr (x:_) _ = x
headOr _     x = x

singleOr :: [a] -> a -> a
singleOr [x] _ = x
singleOr _   x = x
