{-# OPTIONS_GHC -fno-warn-missing-signatures    #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-unused-binds          #-}

module Main where
import Frontier.Model.Core.DynamicTest
import Frontier.Model.Core.Features.BaseTest
import Frontier.Model.Core.Features.BuildingTest
import Frontier.Model.InteractionTest


import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

main :: IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [featuresBaseTest
    ]
