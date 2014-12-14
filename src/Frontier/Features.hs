{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontier.Features
    (Thing()
    ,withFeatures
    ) where

import Control.Lens (Prism')
import Control.Lens.TH (makePrisms)
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