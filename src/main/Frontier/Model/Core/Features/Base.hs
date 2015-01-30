{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
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

deriving instance (Show (Component a))

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
name WallTag                    = "Wall"
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
    init = compose . concat $
         [  [create
                Object
                PlayerCharacterTag
                ((_position     .~ (1,1))
                .(_symbol       .~ '@')
                .(_zIndex       .~ -1000))
            ]
         ,  [create Object WallTag ((_position .~ (i,0))
                                   .(_symbol   .~ '#'))
            | i <- [0..79]
            ]
         ,  [create Object WallTag ((_position .~ (i,22))
                                   .(_symbol   .~ '#'))
            | i <- [0..79]
            ]
         ,  [create Object WallTag ((_position .~ (0,i))
                                   .(_symbol   .~ '#'))
            | i <- [1..21]
            ]
         ,  [create Object WallTag ((_position .~ (79,i))
                                   .(_symbol   .~ '#'))
            | i <- [1..21]
            ]
         ]
    command :: String -> Action w
    -- Moving
    command c | c `elem` ["h", "j", "k", "l"] =
        withAll Object $ \objs -> compose
            [ modify Object move pc
            | pc <- objs
            , (pc ^# _com) `matches` _PlayerCharacter
            , noCollision env (move pc) (filter (not . isWorldItem) objs)
            ]
      where
        isWorldItem e | WorldItem _ <- e ^# _com = True
        isWorldItem _                            = False
        move = case c of
            (x:_)   -> moveToDir env x
            _       -> id
    -- Show inventory
    command "i" =
        withAll Item $ \items ->
            message
                . flip (++)
                . (:[])
                . intercalate "\n"
                . zipWith annotate itemHandles
                . combineEquals
                . sort
                . map (name . (^. _tag))
                $ items
            where
              annotate handle (item, count)
                | count == 1
                    = handle : " - " ++ item
                | otherwise
                    = handle : " - " ++ item ++ " (x" ++ show count ++ ")"
    -- Object pickup
    command "p" =
        withAll Object $ \objs -> compose
            [ destroy Object obj
            . create Item (getItemTag obj) id
            . message (++ ["Picked up " ++ (name . getItemTag) obj ++ "."])
            | obj <- objs
            , pc <- objs
            , (obj ^# _com) `matches` _WorldItem
            , (pc ^# _com) `matches` _PlayerCharacter
            , pc^._position == obj^._position
            ]
      where
        getItemTag obj | WorldItem s <- obj ^# _com = s
        getItemTag _ = error "Base.hs: Trying to get itemTag of non-WorldItem"
    command _ = id

    step :: Action w
    step =
        -- Clear previous messages
        message (const [])
