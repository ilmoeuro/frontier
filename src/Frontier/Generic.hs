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

import Data.Maybe (catMaybes)
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

delegateObject :: (forall o i. Fe.Feature o i -> o -> a) -> Object -> a
delegateObject fn (BuildingObject o)      = fn Building.feature o
delegateObject fn (FarmingObject o)       = fn Farming.feature o

delegateItem :: (forall o i. Fe.Feature o i -> i -> a) -> Item -> a
delegateItem fn (BuildingItem i)        = fn Building.feature i
delegateItem fn (FarmingItem i)         = fn Farming.feature i

fmapObjects :: Functor f 
            => (forall o i. Fe.Feature o i -> f o) 
            -> [f Object]
fmapObjects fn =
    [BuildingObject     <$> fn Building.feature
    ,FarmingObject      <$> fn Farming.feature
    ]

fmapItems :: Functor f 
          => (forall o i. Fe.Feature o i -> f i) 
          -> [f Item]
fmapItems fn =
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
initialItems = join $ fmapItems Fe.initialItems
    
symbol :: Object -> Char
symbol = delegateObject Fe.symbol
    
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