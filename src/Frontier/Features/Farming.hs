{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
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
feature = Feature{..} where
    componentFor    E.Blank                 = Blank
    componentFor    _                       = Dummy

    initItems                               = []

    symbol          _                       = ""

    command         _           _           = []

    doTurn          _           _           = []

    eq              Blank       _           = True
    eq              _           Blank       = True
    eq              a           b           = a == b

    partialUpdate   Blank       x           = x
    partialUpdate   x           _           = x
