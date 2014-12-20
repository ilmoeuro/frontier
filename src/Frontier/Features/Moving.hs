{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
module Frontier.Features.Moving
    (Component()
    ,feature) where

-- import Control.Monad
import Frontier.Feature
import Frontier.Feature.Action
import qualified Frontier.Feature.Entity as E
-- import Frontier.Feature.Qualifier

data Component a where
    PlayerCharacter     :: Component a
    Dummy               :: Component a

deriving instance Show (Component a)
deriving instance Eq (Component a)

feature :: Feature Component
feature = \case
    (ComponentFor E.PlayerCharacter)-> PlayerCharacter
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
    (Command _ _)                   -> []

    (DoTurn _ _)                    -> []

    (Symbol PlayerCharacter)        -> "@"
    (Symbol Dummy)                  -> ""

    (Eq a b)                        -> a == b
