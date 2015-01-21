{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Managed
import Data.Default
import Graphics.Vty
import MVC hiding (Input, Output)

getVty :: Managed Vty
getVty = managed $ \inside -> do
    vty@Vty{shutdown} <- mkVty def
    result <- inside vty
    shutdown
    return result

main :: IO ()
main = undefined
