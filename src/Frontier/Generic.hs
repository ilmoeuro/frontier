{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
module Frontier.Generic
    (Object()
    ,Item()

    ,use
    ,initialItems
    ,symbol

    ,Alike(..)
    ) where

import Control.Monad
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

delegateO :: (forall o i. Fe.Feature o i -> o -> a) -> Object -> a
delegateO fn (BuildingObject o)      = fn Building.feature o
delegateO fn (FarmingObject o)       = fn Farming.feature o

{-# ANN delegateI "HLint: ignore" #-} -- unused
delegateI :: (forall o i. Fe.Feature o i -> i -> a) -> Item -> a
delegateI fn (BuildingItem i)        = fn Building.feature i
delegateI fn (FarmingItem i)         = fn Farming.feature i

{-# ANN fmapO "HLint: ignore" #-} -- unused
fmapO :: Functor f 
      => (forall o i. Fe.Feature o i -> f o) 
      -> [f Object]
fmapO fn =
    [BuildingObject     <$> fn Building.feature
    ,FarmingObject      <$> fn Farming.feature
    ]

fmapI :: Functor f 
      => (forall o i. Fe.Feature o i -> f i) 
      -> [f Item]
fmapI fn =
    [BuildingItem       <$> fn Building.feature
    ,FarmingItem        <$> fn Farming.feature
    ]

-- The feature functions in generic form

use :: Item -> ActionM Item Object ()
-- Partial functions OK here because we only get out
-- what we put in.
use (BuildingItem i) = transform
        BuildingItem
        (\(BuildingItem i') -> i')
        BuildingObject
        (\(BuildingObject o') -> o')
        (Fe.use Building.feature i)
use (FarmingItem i) = transform
        FarmingItem
        (\(FarmingItem i') -> i')
        FarmingObject
        (\(FarmingObject o') -> o')
        (Fe.use Farming.feature i)

initialItems :: [Item]
initialItems = join $ fmapI Fe.initialItems
    
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