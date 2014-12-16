{-# LANGUAGE GADTSyntax          #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE TemplateHaskell     #-}
module Frontier.Features
    (Generic()
    ,withFeatures
    ,collect
    ,collectP
    ,collectF
    ,collectPF
    ,dispatch
    ,dispatchP
    ) where

import Data.Maybe
import Data.Functor.Identity
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

deriving instance Show (Generic a)
    
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

collect :: (forall a. Feature a -> a c)
          -> [Generic c]
collect f = let f' a _ = f a in collectP f'

collectP :: (forall a. 
                Feature a
                -> (forall b'. Prism' (Generic b') (a b'))
                -> a c)
           -> [Generic c]
collectP f = map runIdentity $ collectPF (((.).(.)) Identity f)

collectF :: Functor f
         => (forall a. Feature a -> f (a c))
         -> [f (Generic c)]
collectF f = let f' a _ = f a in collectPF f'

collectPF :: Functor f
          => (forall a. 
                Feature a
                -> (forall b'. Prism' (Generic b') (a b'))
                -> f (a c))
           -> [f (Generic c)]
collectPF f = withFeatures $ \ftr pr -> (^. re pr) `fmap` f ftr pr
    
dispatch :: (forall a. Feature a -> a b -> c)
         -> Generic b
         -> c
dispatch f x = let f' a _ = f a in dispatchP f' x 
    
dispatchP :: (forall a. Feature a 
              -> (forall b'. Prism' (Generic b') (a b')) 
              -> a b 
              -> c)
          -> Generic b
          -> c
dispatchP f x =
    -- OK to use here because one module always
    -- knows how to handle an object
    (`singleOr` error "feature dispatch failed")
    . catMaybes
    $ withFeatures 
    $ \ftr pr -> x ^? pr <&> f ftr pr