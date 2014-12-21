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
import qualified Frontier.Feature.Entity as E
-- import Frontier.Feature.Qualifier

data Component a where
    Blank               :: Component a
    Dummy               :: Component a

deriving instance Show (Component a)
deriving instance Eq (Component a)

feature :: Feature Component
feature = \case
    (ComponentFor E.Blank)          -> Blank
    (ComponentFor _)                -> Dummy

    InitItems                       -> []

    (Symbol _)                      -> ""

    (Command _ _)                   -> []

    (DoTurn _ _)                    -> []

    (Eq _ _)                        -> error "should be shadowed"

    (PartialUpdate _ _)             -> error "should be shadowed"
