{-# OPTIONS_GHC -fno-warn-missing-signatures    #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-unused-binds          #-}

module Main where
import Frontier.Core.DynamicTest
import Frontier.Core.Features.BaseTest
import Frontier.Core.Features.BuildingTest
import Frontier.InteractionTest


import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

main :: IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [
    ]
