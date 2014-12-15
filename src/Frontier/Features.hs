{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontier.Features
    (Generic()
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

data Generic a where
    BuildingSpecific    :: Building.Specific a      -> Generic a
    FarmingSpecific     :: Farming.Specific a       -> Generic a
    MovingSpecific      :: Moving.Specific a        -> Generic a
    
makePrisms ''Generic

withFeatures :: (forall a. Feature a
                -> (forall b. Prism' (Generic b) (a b))
                -> c)
             -> [c]
withFeatures f =
    [f Building.feature     _BuildingSpecific
    ,f Farming.feature      _FarmingSpecific
    ,f Moving.feature       _MovingSpecific
    ]

covariant :: (forall a. Feature a -> a c)
          -> [Generic c]
covariant f = withFeatures $ \ftr pr -> f ftr ^. re pr
    
contravariant :: (forall a. Feature a -> a b -> c)
              -> Generic b
              -> c
contravariant f x =
    -- OK to use here because one module always
    -- knows how to handle an object
    (`singleOr` error "error in contravariant: module dispatch failed")
    . catMaybes
    $ withFeatures 
    $ \ftr pr -> x ^? pr <&> f ftr