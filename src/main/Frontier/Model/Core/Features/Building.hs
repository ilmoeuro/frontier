{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
module Frontier.Model.Core.Features.Building
    (Component()
    ,fromTag
    ,feature
    ) where

import Control.Lens hiding (contains)
import Frontier.Model.Core.Feature
import Frontier.Model.Core.Feature.Prelude
import Frontier.Prelude
import Prelude hiding (init)
import System.Random

data Component a where
    PlayerCharacter             :: Component Object
    Tree                        :: Component Object
    Hammer                      :: Component Item
    Axe                         :: Component Item
    Lumber                      :: Component Item
    Unknown                     :: Component a

deriving instance Eq (Component a)

_PlayerCharacter :: Prism' (Component Object) ()
_PlayerCharacter = prism'
    (const PlayerCharacter)
    (\case
        PlayerCharacter         -> Just ()
        _                       -> Nothing)

_Tree :: Prism' (Component Object) ()
_Tree = prism'
    (const Tree)
    (\case
        Tree                    -> Just ()
        _                       -> Nothing)

_Hammer :: Prism' (Component Item) ()
_Hammer = prism'
    (const Hammer)
    (\case
        Hammer                  -> Just ()
        _                       -> Nothing)

_Axe :: Prism' (Component Item) ()
_Axe = prism'
    (const Axe)
    (\case
        Axe                     -> Just ()
        _                       -> Nothing)

_Unknown :: Prism' (Component Object) ()
_Unknown = prism'
    (const Unknown)
    (\case
        Unknown                 -> Just ()
        _                       -> Nothing)

fromTag :: Tag b -> Component b
fromTag PlayerCharacterTag     =  PlayerCharacter
fromTag HammerTag              =  Hammer
fromTag AxeTag                 =  Axe
fromTag LumberTag              =  Lumber
fromTag _                      =  Unknown

feature :: forall e w. Env w e
        -> (forall b. ALens' (e b) (Component b))
        -> Feature w
feature env@Env{..} _com = Feature {..} where

    _com' :: Lens' (e b) (Component b)
    _com' = cloneLens _com

    init :: Action w
    init = withInitParam $ \initParam ->
           create
            Object
            (WorldItemTag HammerTag)
            ((_position     .~ (5,5))
            .(_symbol       .~ '/')
            .(_size         .~ Small))
         . create
            Object
            (WorldItemTag AxeTag)
            ((_position     .~ (9,7))
            .(_symbol       .~ '/')
            .(_size         .~ Small))
         . (foldr ((.) . mkTree) id
           . take 100
           $ zip (randoms . mkStdGen $ initParam)
                 (randoms . mkStdGen . (+1) $ initParam))
      where
        mkTree (x', y') =
            create Object OpaqueTag
                ((_position     .~ (x' `rem` 80, y' `rem` 23))
                .(_symbol       .~ '^')
                .(_com          #~ Tree))

    command :: String -> Action w
    -- Building
    command c | c `elem` ["Bh", "Bj", "Bk", "Bl"] = build where
        filterByComponent p = filter (\e -> p (e ^# _com))
        move = case c of
            (_:x:_)     -> moveToDir env x
            _           -> id
        createWall = withAll Object $ \objs -> compose
            [create Object OpaqueTag
                ( move
                . (_position   .~ (obj ^. _position))
                . (_symbol     .~ '#'))
            | obj <- objs
            , (obj ^# _com) `matches` _PlayerCharacter
            , noCollision env (move obj) objs
            ]
        build = withAll Item $ \items ->
            when' (not . null . filterByComponent (== Hammer) $ items)
            $ case filterByComponent (== Lumber) items of
                (lumber:_)  ->
                    destroy Item lumber
                    . createWall
                _ -> id
    -- Chopping
    command c | c `elem` ["Ch", "Cj", "Ck", "Cl"] =
        withAll Object $ \objs -> compose
            [ destroy Object obj
            . create Item LumberTag id
            | obj <- objs
            , pc <- objs
            , (obj ^# _com) `matches` _Tree
            , (pc ^# _com) `matches` _PlayerCharacter
            , move pc^._position == obj^._position
            ]
      where
        move = case c of
            (_:x:_)     -> moveToDir env x
            _           -> id
    command _ = id

    step :: Action w
    step = id
