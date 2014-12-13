{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Frontier.Generic
    (Generic()
    ,GenericAction()

    ,FeatureTag(..)
    ,featureTag
    
    ,initItems
    ,symbol
    ,action
    ,initPlayerCharacter
    
    ,runAction
    ) where

import Control.Applicative
import Frontier.Feature.Qualifier
import Frontier.Feature.Action (ActionM)
import Frontier.Feature (Feature)
import qualified Frontier.Feature as Fe
import qualified Frontier.Features.Building as Building
import qualified Frontier.Features.Farming as Farming
import qualified Frontier.Features.Moving as Moving

-- Generic types

data Generic a where
    BuildingThing   :: Building.Thing a         -> Generic a
    FarmingThing    :: Farming.Thing a          -> Generic a
    MovingThing     :: Moving.Thing a           -> Generic a
    
data GenericAction a = GenericAction
    {buildingAction :: ActionM Building.Thing a
    ,farmingAction  :: ActionM Farming.Thing a
    ,movingAction   :: ActionM Moving.Thing a
    }
    
-- Helper functions

covariant :: (forall a. Feature a -> a b) -> [Generic b]
covariant f =
    [BuildingThing      $ f Building.feature
    ,FarmingThing       $ f Farming.feature
    ,MovingThing        $ f Moving.feature
    ]
    
covariantF :: Functor f
            => (forall a. Feature a -> f (a b))
            -> [f (Generic b)]
covariantF f =
    [BuildingThing      <$> f Building.feature
    ,FarmingThing       <$> f Farming.feature
    ,MovingThing        <$> f Moving.feature
    ]
    
contravariant :: (forall a. Feature a -> a b -> c) -> Generic b -> c
contravariant fn (BuildingThing x)  = fn Building.feature   x
contravariant fn (FarmingThing x)   = fn Farming.feature    x
contravariant fn (MovingThing x)    = fn Moving.feature     x

covariantAction :: (forall a. Feature a -> ActionM a b) -> GenericAction b
covariantAction f = GenericAction
    {buildingAction     = f Building.feature
    ,farmingAction      = f Farming.feature
    ,movingAction       = f Moving.feature
    }

-- The feature functions in generic form

initItems :: [Generic Item]
initItems = concat $ covariantF Fe.initItems

symbol :: Generic Object -> Char
symbol = contravariant Fe.symbol

action :: Char -> GenericAction ()
action c = covariantAction $ flip Fe.action c

initPlayerCharacter :: [Generic Object]
initPlayerCharacter = covariant Fe.initPlayerCharacter

-- Meta functions

-- TODO: DRY; unwrapXXX are prisms
runAction :: (forall a. 
                    ActionM a b ->
                    (forall d. Generic d -> Maybe (a d)) ->
                    c)
              -> GenericAction b 
              -> [c]
runAction f GenericAction{..} =
    [f buildingAction unwrapBuilding
    ,f farmingAction unwrapFarming
    ,f movingAction unwrapMoving
    ]
    where
        unwrapBuilding  (BuildingThing a)   = Just a
        unwrapBuilding  _                   = Nothing
        unwrapFarming   (FarmingThing a)    = Just a
        unwrapFarming   _                   = Nothing
        unwrapMoving    (MovingThing a)     = Just a
        unwrapMoving    _                   = Nothing

-- Tagging

data FeatureTag
    = BuildingTag
    | FarmingTag
    | MovingTag
    
featureTag :: Generic a -> FeatureTag
featureTag (BuildingThing _)    = BuildingTag
featureTag (FarmingThing _)     = FarmingTag
featureTag (MovingThing _)      = MovingTag