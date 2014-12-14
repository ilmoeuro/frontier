{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontier.Generic
    (Generic()
    ,Context(..)
    ,withContext
    ) where

import Control.Lens (Prism')
import Control.Lens.TH (makePrisms)
import Frontier.Feature (Feature)
import qualified Frontier.Features.Building as Building
import qualified Frontier.Features.Farming as Farming
import qualified Frontier.Features.Moving as Moving

data Generic a where
    BuildingThing   :: Building.Thing a         -> Generic a
    FarmingThing    :: Farming.Thing a          -> Generic a
    MovingThing     :: Moving.Thing a           -> Generic a
    
makePrisms ''Generic

data Context a = Context
    {feature        :: Feature a
    ,prism          :: forall b. Prism' (Generic b) (a b)
    }

withContext :: (forall a. Context a -> b) -> [b]
withContext f =
    [f $ Context Building.feature       _BuildingThing
    ,f $ Context Farming.feature        _FarmingThing
    ,f $ Context Moving.feature         _MovingThing
    ]