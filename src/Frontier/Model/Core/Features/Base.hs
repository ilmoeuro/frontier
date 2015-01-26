{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontier.Model.Core.Features.Base
    (Component()
    ,fromTag
    ,feature
    ) where

import Control.Lens
import Data.List hiding (init)
import Frontier.Model.Core.Feature
import Frontier.Model.Core.Feature.Prelude
import Frontier.Prelude
import Prelude hiding (init)

data Component a where
    PlayerCharacter             :: Component Object
    WorldItem                   :: Tag Item -> Component Object
    Unknown                     :: Component a

_PlayerCharacter :: Prism' (Component Object) ()
_PlayerCharacter = prism'
    (const PlayerCharacter)
    (\case
        PlayerCharacter -> Just ()
        _               -> Nothing)

_WorldItem :: Prism' (Component Object) (Tag Item)
_WorldItem = prism'
    WorldItem
    (\case
        WorldItem x     -> Just x
        _               -> Nothing)

_Unknown :: Prism' (Component Object) ()
_Unknown = prism'
    (const Unknown)
    (\case
        Unknown         -> Just ()
        _               -> Nothing)

fromTag :: Tag b -> Component b
fromTag PlayerCharacterTag      =  PlayerCharacter
fromTag (WorldItemTag x)        =  WorldItem x
fromTag _                       =  Unknown

name :: Tag b -> String
name PlayerCharacterTag         = "Player character"
name HammerTag                  = "Hammer"
name AxeTag                     = "Axe"
name LumberTag                  = "Lumber"
name (WorldItemTag x)           = "Item: " ++ name x
name OpaqueTag                  = "???"

feature :: forall e w. Env w e
        -> (forall b. ALens' (e b) (Component b))
        -> Feature w
feature env@Env{..} _com = Feature {..} where

    init :: Action w
    init = create
            Object
            PlayerCharacterTag
            ((_position     .~ (1,1))
            .(_symbol       .~ '@'))

    command :: String -> Action w
    -- Moving
    command c | c `elem` ["h", "j", "k", "l"] =
        withAll Object $ \objs -> compose
            [ modify Object move pc
            | pc <- objs
            , (pc ^# _com) `matches` _PlayerCharacter
            , noCollision env (move pc) objs
            ]
      where
        move = case c of
            (x:_)   -> moveToDir env x
            _       -> id
    -- Show inventory
    command c | c == "!" =
        withAll Item $ \items ->
            message
                . (++)
                . (:[])
                . intercalate "\n"
                $   [ show num ++ " - " ++ name (item ^. _tag)
                    | (num, item) :: (Int, e Item) <- zip [1..] items
                    ]
    -- Object pickup
    command c | c `elem` ["ph", "pj", "pk", "pl"] =
        withAll Object $ \objs -> compose
            [ destroy Object obj
            . create Item (getItemTag obj) id
            . message (++ ["Picked up " ++ (name . getItemTag) obj])
            | obj <- objs
            , pc <- objs
            , (obj ^# _com) `matches` _WorldItem
            , (pc ^# _com) `matches` _PlayerCharacter
            , move pc^._position == obj^._position
            ]
      where
        getItemTag obj | WorldItem s <- obj ^# _com = s
        getItemTag _ = error "Base.hs: Trying to get tag of non-WorldItem"
        move = case c of
            (_:x:_) -> moveToDir env x
            _       -> id
    command _ = id

    step :: Action w
    step = message (const [])
