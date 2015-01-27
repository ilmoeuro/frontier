{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
-- Horrible, horrible thing.
module Frontier.Model.Core.Testing.Instances() where

import Control.Applicative
import Frontier.Model.Core.Feature
import Frontier.Model.Core.Testing.Static
import Test.QuickCheck

instance Show (Meta Object) where
    show (ObjectMeta pos char size)
        = "ObjectMeta "
        ++ show pos ++ " "
        ++ show char ++ " "
        ++ show size
instance Show (Meta Item) where
    show ItemMeta               = "ItemMeta"

instance Arbitrary (Tag Item) where
    arbitrary = oneof
        [pure HammerTag
        ,pure AxeTag
        ,pure LumberTag
        ,pure OpaqueTag
        ]

instance Arbitrary (Tag Object) where
    arbitrary = oneof
        [pure PlayerCharacterTag
        ,WorldItemTag <$> arbitrary
        ,pure OpaqueTag
        ]

deriving instance Show (c Object) => Show (Entity c Object)
deriving instance Show (c Item) => Show (Entity c Item)
deriving instance (Show (c Object), Show (c Item)) => Show (World c)
