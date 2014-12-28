{-# LANGUAGE FlexibleContexts #-}
module Frontier.Engine.Pipe
    (frontierPipe
    ) where

import Control.Monad.State
import Frontier.Engine.State
import Pipes
import qualified Pipes.Prelude as P

data Input
    = KeyPress Char
    deriving (Read)

data Output
    = WorldWindow String
    deriving (Show)
    
frontierPipe :: 
              (Monad m 
              ,MonadState a m
              )
             => Pipe Input Output m ()
frontierPipe = do
    input <- await
    case input of
        KeyPress 'q'    -> return ()
        _               -> do
            yield (WorldWindow "")
            frontierPipe
            
runPipe :: IO ()
runPipe = execStateT
    (runEffect (P.readLn >-> frontierPipe >-> P.print))
    ()