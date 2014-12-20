{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Frontier.Features.Farming
    (Component()
    ,feature
    ) where

-- import Control.Monad
import Frontier.Feature
-- import qualified Frontier.Feature.Entity as E
-- import Frontier.Feature.Qualifier

data Component a where
    Dummy               :: Component a

deriving instance Show (Component a)
deriving instance Eq (Component a)

feature :: Feature Component
feature = \case
    (ComponentFor _)                -> Dummy

    InitItems                       -> []

    (Symbol Dummy)                  -> ""

    (Command _ _)                   -> []

    (Eq a b)                        -> a == b
