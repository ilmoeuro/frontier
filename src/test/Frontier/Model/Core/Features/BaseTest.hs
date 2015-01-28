{-# OPTIONS_GHC -fno-warn-missing-signatures    #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-unused-binds          #-}

{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PatternGuards      #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
module Frontier.Model.Core.Features.BaseTest
    (featuresBaseTest
    ) where

import Control.Applicative
import Control.Lens hiding (elements)
import Data.Function
import Data.List hiding (init)
import Debug.Trace
import Prelude hiding (init)

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Frontier.Model.Core.Feature (Item, Object, Tag (..), Witness (..))
import qualified Frontier.Model.Core.Feature as Ftr
import Frontier.Model.Core.Testing.Dynamic
import Frontier.Model.Core.Testing.Instances
import Frontier.Model.Core.Testing.Static

import Frontier.Model.Core.Features.Base

Ftr.Env{..} = env fromTag
Ftr.Feature{..} = feature (env fromTag) _component

newtype WorldWithPlayerCharacter
    = WorldWithPlayerCharacter (World Component)

newtype MovementCommands
    = MovementCommands [String]
    deriving (Show)

deriving instance Show (WorldWithPlayerCharacter)

instance Arbitrary WorldWithPlayerCharacter where
    arbitrary
        = fmap
          (\(itemTags, objectTags, positions, playerPos) ->
              WorldWithPlayerCharacter
              . foldr ($) mkWorld
              . concat
              $
              [   [create Item tag id
                  | tag <- itemTags
                  ]
              ,   [create Object tag (_position .~ pos)
                  | (tag, pos) <- zip objectTags
                                . nub
                                . filter (/= clamp playerPos)
                                . map clamp
                                $ positions
                  , tag /= PlayerCharacterTag
                  ]
              ,   [create Object
                       PlayerCharacterTag
                       (_position .~ clamp playerPos)
                  ]
              ])
          arbitrary
        where
          clamp (x,y) = (abs x `rem` 5, abs y `rem` 5)

instance Arbitrary MovementCommands where
    arbitrary = MovementCommands <$> (listOf . elements $ ["h","j","k","l"])

featuresBaseTest = testGroup "Frontier.Model.Core.Features.Base"
    [testCase "Player character present after init"
        $ ((Ftr.PlayerCharacterTag `elem`)
            . map entityTag
            . objects
            $ init mkWorld)
        @? "No object with PlayerCharacterTag"

    ,QC.testProperty "Messages cleared on step"
        $ \(WorldWithPlayerCharacter world) msg ->
        null . messages . step . message (++msg) $ world

    ,QC.testProperty "Move doesn't cause collisions"
        $ \(WorldWithPlayerCharacter world) (MovementCommands cmds) ->
        let world' = foldr ((.) . command) id cmds world
            positions = map (view _position)
                      . filter (not . isWorldItem)
                      . objects
                      $ world'
            isWorldItem e | WorldItemTag _ <- entityTag e = True
            isWorldItem _                                 = False
        in positions === nub positions
    ]
