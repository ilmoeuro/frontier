{-# LANGUAGE LambdaCase #-}
module Frontier.Model.Dynamic
    (FrontierM
    ,run
    ) where

import Control.Monad
import Control.Monad.State
import Frontier.Model.Dynamic.Actions
import Frontier.Model.Dynamic.Interface
import Pipes

run :: FrontierM ()
run = forever $ await >>= \case
    (Move d)    -> move d >> get >>= display
    (Chop d)    -> chop d >> get >>= display
    (Build d)   -> build d >> get >>= display
