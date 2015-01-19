{-# LANGUAGE LambdaCase #-}
module Frontier.Model.Dynamic
    (FrontierM
    ,run
    ) where

import Control.Monad.State
import Frontier.Model.Dynamic.Actions
import Frontier.Model.Dynamic.Interface
import Pipes

run :: FrontierM ()
run = get >>= display >> go
  where
    go = await >>= \case
        (Move d)    -> move d >> continue
        (Chop d)    -> chop d >> continue
        (Build d)   -> build d >> continue
        (Smash d)   -> smash d >> continue
        (Query d)   -> do msg <- query d
                          world <- get
                          message msg world
                          go
        (Unbox d)   -> unbox d >> continue
        Quit        -> return ()
    continue = get >>= display >> go
