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

fromTag :: Tag b -> Component b
fromTag PlayerCharacterTag     =  PlayerCharacter
fromTag HammerTag              =  Hammer
fromTag AxeTag                 =  Axe
fromTag LumberTag              =  Lumber
fromTag _                      =  Unknown

welcomeMessage :: [String]
welcomeMessage =
    ["  C   - chop (takes a direction as argument)"
    ,"  B   - build (takes a direction as argument)"
    ]

feature :: forall e w. Env w e
        -> (forall b. ALens' (e b) (Component b))
        -> Feature w
feature env@Env{..} _com = Feature {..} where

    _com' :: Lens' (e b) (Component b)
    _com' = cloneLens _com

    init :: Action w
    init = withInitParam $ \initParam ->
           message ([unlines welcomeMessage] ++)
         . create
            Object
            (WorldItemTag HammerTag)
            ((_position     .~ (5,5))
            .(_symbol       .~ '/'))
         . create
            Object
            (WorldItemTag AxeTag)
            ((_position     .~ (9,7))
            .(_symbol       .~ '/'))
         . (foldr ((.) . mkTree) id
           . take 100
           $ zip (randoms . mkStdGen $ initParam)
                 (randoms . mkStdGen . (+1) $ initParam))
      where
        mkTree (x', y') =
            create Object OpaqueTag
                ((_position     .~ (1 + abs x' `rem` 78, 1 + abs y' `rem` 21))
                .(_symbol       .~ '^')
                .(_com          #~ Tree))

    command :: String -> Action w
    -- Help message
    command "?" = message ([unlines welcomeMessage] ++)
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
            , obj ^# _com == PlayerCharacter
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
            , obj ^# _com == Tree
            , pc ^# _com == PlayerCharacter
            , move pc^._position == obj^._position
            ]
      where
        move = case c of
            (_:x:_)     -> moveToDir env x
            _           -> id
    command _ = id

    step :: Action w
    step = id

    loadLevel :: LevelSource -> Action w
    loadLevel = const id
