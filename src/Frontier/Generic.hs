{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
module Frontier.Generic
    (Object()
    ,Item()

    ,action
    ,initialItems
    ,symbol

    ,Alike(..)
    ) where

import Control.Applicative
import Frontier.Feature.Action (ActionM, transform)
import qualified Frontier.Feature as Fe
import qualified Frontier.Features.Building as Building
import qualified Frontier.Features.Farming as Farming

-- Generic types

data Object where
    BuildingObject  :: Building.Object  -> Object
    FarmingObject   :: Farming.Object   -> Object
    
data Item where
    BuildingItem    :: Building.Item    -> Item
    FarmingItem     :: Farming.Item     -> Item
    
-- Helper functions

delegateO :: (forall i o. Fe.Feature i o -> o -> a) -> Object -> a
delegateO fn (BuildingObject o)      = fn Building.feature o
delegateO fn (FarmingObject o)       = fn Farming.feature o

{-# ANN delegateI "HLint: ignore" #-} -- unused
delegateI :: (forall i o. Fe.Feature i o -> i -> a) -> Item -> a
delegateI fn (BuildingItem i)        = fn Building.feature i
delegateI fn (FarmingItem i)         = fn Farming.feature i

{-# ANN fmapO "HLint: ignore" #-} -- unused
fmapO :: Functor f 
      => (forall i o. Fe.Feature i o -> f o) 
      -> [f Object]
fmapO fn =
    [BuildingObject     <$> fn Building.feature
    ,FarmingObject      <$> fn Farming.feature
    ]

fmapI :: Functor f 
      => (forall i o. Fe.Feature i o -> f i) 
      -> [f Item]
fmapI fn =
    [BuildingItem       <$> fn Building.feature
    ,FarmingItem        <$> fn Farming.feature
    ]
    
delegateA :: (forall i o. Fe.Feature i o -> ActionM i o ())
          -> [ActionM Item Object ()]
-- Partial functions OK here because we only get out
-- what we put in.
delegateA act =
    [transform
        BuildingItem
        (\(BuildingItem i) -> i)
        BuildingObject
        (\(BuildingObject o) -> o)
        (act Building.feature)
    ,transform
        FarmingItem
        (\(FarmingItem i) -> i)
        FarmingObject
        (\(FarmingObject o) -> o)
        (act Farming.feature)
    ]

-- The feature functions in generic form

action :: Char -> [ActionM Item Object ()]
action c = delegateA $ \ftr -> Fe.action ftr c

initialItems :: [Item]
initialItems = concat $ fmapI Fe.initialItems
    
symbol :: Object -> Char
symbol = delegateO Fe.symbol
    
-- Meta functions

class Alike a where
    (~~) :: a -> a -> Bool
    (/~) :: a -> a -> Bool
    (/~) = ((.).(.)) not (~~)
    
instance Alike Object where
    (~~) (BuildingObject _)     (BuildingObject _)      = True
    (~~) (FarmingObject _)      (FarmingObject _)       = True
    (~~) _                      _                       = False

instance Alike Item where
    (~~) (BuildingItem _)      (BuildingItem _)         = True
    (~~) (FarmingItem _)       (FarmingItem _)          = True
    (~~) _                      _                       = False