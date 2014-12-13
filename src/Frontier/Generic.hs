{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
module Frontier.Generic
    (Generic()

    ,FeatureTag(..)
    ,featureTag
    
    ,initItems
    ,symbol
    ,action
    ,initPlayerCharacter
    ) where

import Control.Applicative
import Frontier.Feature.Qualifier
import Frontier.Feature.Action (ActionM, transform)
import Frontier.Feature (Feature)
import qualified Frontier.Feature as Fe
import qualified Frontier.Features.Building as Building
import qualified Frontier.Features.Farming as Farming

-- Generic type

data Generic a where
    BuildingThing  :: Building.Thing a      -> Generic a
    FarmingThing   :: Farming.Thing a       -> Generic a

-- Meta functions

data FeatureTag
    = BuildingTag
    | FarmingTag
    
featureTag :: Generic a -> FeatureTag
featureTag (BuildingThing _)    = BuildingTag
featureTag (FarmingThing _)     = FarmingTag
    
-- Helper functions

covariant :: (forall a. Feature a -> a b) -> [Generic b]
covariant f =
    [BuildingThing      $ f Building.feature
    ,FarmingThing       $ f Farming.feature
    ]
    
covariantF :: Functor f
            => (forall a. Feature a -> f (a b))
            -> [f (Generic b)]
covariantF f =
    [BuildingThing      <$> f Building.feature
    ,FarmingThing       <$> f Farming.feature
    ]
    
contravariant :: (forall a. Feature a -> a b -> c) -> Generic b -> c
contravariant fn (BuildingThing x)  = fn Building.feature   x
contravariant fn (FarmingThing x)   = fn Farming.feature    x

-- The feature functions in generic form

initItems :: [Generic Item]
initItems = concat $ covariantF Fe.initItems

symbol :: Generic Object -> Char
symbol = contravariant Fe.symbol

action :: Char -> [ActionM Generic ()]
action c =
    -- Partial functions OK here because
    -- we only get out what we put in
    [transform
        BuildingThing
        (\(BuildingThing x) -> x)
        (Fe.action Building.feature c)
    ,transform
        FarmingThing
        (\(FarmingThing x) -> x)
        (Fe.action Farming.feature c)
    ]

initPlayerCharacter :: [Generic Object]
initPlayerCharacter = covariant Fe.initPlayerCharacter