{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Frontier.Features.Moving
    (Component()
    ,feature) where

-- import Control.Monad
import Frontier.Feature
import Frontier.Feature.Action
-- import Frontier.Feature.Qualifier

data Component a where
    Dummy               :: Component a

deriving instance Show (Component a)
deriving instance Eq (Component a)

feature :: Feature Component
feature = \case
    (ComponentFor _)                -> Dummy

    InitItems                       -> []

    (Command 'h' fn)                -> (:[]) . fn $ do
        shortDescription "Move east"
        me >>= move E
    (Command 'j' fn)                -> (:[]) . fn $ do
        shortDescription "Move south"
        me >>= move S
    (Command 'k' fn)                -> (:[]) . fn $ do
        shortDescription "Move north"
        me >>= move N
    (Command 'l' fn)                -> (:[]) . fn $ do
        shortDescription "Move west"
        me >>= move W
    (Command _ fn)                  -> (:[]) . fn $ disabled

    (Symbol Dummy)                  -> " "

    (Eq a b)                        -> a == b
