{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontier.Features
    (Thing()
    ,withFeatures
    ,covariant
    ,contravariant
    ) where

import Data.Maybe
import Control.Lens
import Frontier.Extra (singleOr)
import Frontier.Feature (Feature)
import qualified Frontier.Features.Building as Building
import qualified Frontier.Features.Farming as Farming
import qualified Frontier.Features.Moving as Moving

data Thing a where
    BuildingThing   :: Building.Thing a         -> Thing a
    FarmingThing    :: Farming.Thing a          -> Thing a
    MovingThing     :: Moving.Thing a           -> Thing a
    
makePrisms ''Thing

withFeatures :: (forall a. Feature a
                -> (forall b. Prism' (Thing b) (a b))
                -> c)
             -> [c]
withFeatures f =
    [f Building.feature     _BuildingThing
    ,f Farming.feature      _FarmingThing
    ,f Moving.feature       _MovingThing
    ]

covariant :: (forall a. Feature a -> a c)
          -> [Thing c]
covariant f = withFeatures $ \ftr pr -> f ftr ^. re pr
    
contravariant :: (forall a. Feature a -> a b -> c)
              -> Thing b
              -> c
contravariant f x =
    -- OK to use here because one module always
    -- knows how to handle an object
    (`singleOr` error "error in contravariant: module dispatch failed")
    . catMaybes
    $ withFeatures 
    $ \ftr pr -> x ^? pr <&> f ftr