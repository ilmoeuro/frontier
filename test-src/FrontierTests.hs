{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} Frontier.Features.FarmingTest
import {-@ HTF_TESTS @-} Frontier.Features.MovingTest
import {-@ HTF_TESTS @-} Frontier.Features.BuildingTest
import {-@ HTF_TESTS @-} Frontier.Engine.ActionTest
import {-@ HTF_TESTS @-} Frontier.Feature.ComposeTest

main :: IO()
main = htfMain htf_importedTests
