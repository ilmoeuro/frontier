{-# OPTIONS_GHC -fno-warn-missing-signatures    #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-unused-binds          #-}

{-# LANGUAGE RecordWildCards #-}
module Frontier.Model.Core.Features.BuildingTest
    (featuresBuildingTest
    ) where

import Prelude hiding (init)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Frontier.Model.Core.Feature (Env (..), Item, Object, Tag (..),
                                    Witness (..))
import qualified Frontier.Model.Core.Feature as Ftr
import Frontier.Model.Core.Testing.Dynamic
import Frontier.Model.Core.Testing.Static

import Frontier.Model.Core.Features.Building

Ftr.Feature{..} = feature (env fromTag) _component

featuresBuildingTest = testGroup "Frontier.Model.Core.Features.Building"
    [
    ]
