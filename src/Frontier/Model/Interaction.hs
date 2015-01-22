{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE TemplateHaskell  #-}
module Frontier.Model.Interaction
    (Sprite
    ,Input(..)
    ,Output(..)
    ,ModelState()
    ,_Display
    ,mkModelState
    ,model
    ) where

import Control.Lens
import Control.Monad.State.Strict
import Data.Char
import qualified Frontier.Model.Core as Core
import Pipes
import Prelude hiding (init)

type Sprite = ((Int, Int), Char)

data Input
    = KeyChar Char

data Output
    = Display [Sprite]

makePrisms ''Output

newtype ModelState = ModelState { unModelState :: Core.ModelState }
makeLensesFor
    [("unModelState"    ,"_unModelState")
    ]
    ''ModelState

type ModelM = Pipe Input Output (State ModelState)

data Token = TokenCmd String | TokenCount Int

runCore :: Core.Input -> ModelM ()
runCore input =
    lift (zoom _unModelState (unpack `liftM` Core.model input)) >>= yield
  where
    unpack (Core.Display sprites) = Display sprites

mkModelState :: ModelState
mkModelState = ModelState Core.mkModelState

model :: ModelM ()
model = runCore Core.Init >> go
  where
    nullary x       = x `elem` "hjklq'*!\"#¤%&/()=?@{[]}\\+<>,;.:-_"
    unary x         = not (nullary x) && x `elem` ['a'..'z']
    binary x        = x `elem` ['A'..'Z']
    getToken        =
        await >>= \(KeyChar c)   ->
            if | nullary c       -> return . TokenCmd $ [c]
               | unary c         -> await >>= \(KeyChar d) ->
                                    return . TokenCmd $ [c,d]
               | binary c        -> await >>= \(KeyChar d) ->
                                    await >>= \(KeyChar e) ->
                                    return . TokenCmd $ [c,d,e]
               | isDigit c       -> return . TokenCount . read $ [c]
               | otherwise       -> return . TokenCmd $ [c]
    go = runCore Core.Step >>
         getToken >>= \case
            (TokenCmd c)         -> unless (c == "q") $ do
                                        runCore (Core.Command c)
                                        go
            (TokenCount c)       -> getToken >>= \case
                (TokenCmd c')    -> do replicateM_ c (runCore (Core.Command c'))
                                       go
                (TokenCount c')  -> getToken >>= \case
                    (TokenCmd d) -> do replicateM_
                                        ((c * 10) + c')
                                        (runCore (Core.Command d))
                                       go
                    _            -> go

_unused :: ModelState -> Core.ModelState
_unused = unModelState
