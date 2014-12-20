{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Frontier.Engine.Action
    (ActionCtx(..)
    ) where

import Control.Arrow
import Control.Lens hiding (Action)
import Control.Monad
import Control.Monad.Trans.Free
import Data.Maybe
import Frontier.Feature
import Frontier.Feature.Action
import Frontier.Feature.Qualifier


-- Non-prime data & functions are run outside a feature
data ActionCtx a = ActionCtx
    {neighbors          :: [(Direction, a Object)]
    ,inventory          :: [a Item]
    ,this               :: [a Object]
    }
