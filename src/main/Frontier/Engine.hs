{-# LANGUAGE GADTs           #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP             #-}
module Frontier.Engine
    (
#ifdef TEST
     EngineM
    ,Components(..)
    ,Meta(..)
    ,Entity(..)
    ,World(..)
    ,mkWorld
    ,Engine(..)
    ,engine
#else
     EngineM
    ,World()
    ,mkWorld
    ,Engine(..)
    ,engine
#endif
    ) where

import Prelude hiding (init)
import Data.Monoid
import Data.Function
import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as St
import Data.List hiding (init)
import Frontier.Feature hiding (init, command, step)
import qualified Frontier.Feature as Ftr
import qualified Frontier.Feature.Base as Base
import qualified Frontier.Feature.Building as Building

type Id = Int

type Sprite = ((Int, Int), Char)

type EngineM = State World

data Components b = Components
    {base               :: Base.Component b
    ,building           :: Building.Component b
    }

data Meta b where
    ObjectMeta          :: (Int, Int) -> Char -> Int -> Meta Object
    ItemMeta            :: Meta Item

data Entity b = Entity
    {components         :: Components b
    ,meta               :: Meta b
    ,entityTag          :: Tag b
    ,uid                :: Id
    }

data World = World
    {objects            :: [Entity Object]
    ,items              :: [Entity Item]
    ,lastUid            :: Id
    ,initParam          :: Int
    ,messages           :: [String]
    ,dirtyTiles         :: [(Int, Int)]
    }

data Engine = Engine
    {lastMessage        :: EngineM String
    ,allSprites         :: EngineM [Sprite]
    ,changedSprites     :: EngineM [Sprite]
    ,init               :: EngineM ()
    ,command            :: String -> EngineM ()
    ,step               :: EngineM ()
    }

makeLensesFor
    [("base"                ,"_base")
    ,("building"            ,"_building")
    ]
    ''Components

makeLensesFor
    [("components"          ,"_components")
    ,("meta"                ,"_meta")
    ,("entityTag"           ,"_entityTag")
    ]
    ''Entity

makeLensesFor
    [("objects"             ,"_objects")
    ,("items"               ,"_items")
    ,("lastUid"             ,"_lastUid")
    ,("initParam"           ,"_initParam")
    ,("messages"            ,"_messages")
    ,("dirtyTiles"          ,"_dirtyTiles")
    ]
    ''World

__position :: Lens' (Meta Object) (Int, Int)
__position
    = lens  (\(ObjectMeta p _ _)    -> p)
            (\(ObjectMeta _ c z) p  -> ObjectMeta p c z)

__symbol :: Lens' (Meta Object) Char
__symbol
    = lens  (\(ObjectMeta _ c _)    -> c)
            (\(ObjectMeta p _ z) c  -> ObjectMeta p c z)

__zIndex :: Lens' (Meta Object) Int
__zIndex
    = lens  (\(ObjectMeta _ _ z)    -> z)
            (\(ObjectMeta p c _) z  -> ObjectMeta p c z)

mkMeta :: Witness b -> Meta b
mkMeta Object   = ObjectMeta (0,0) '?' 0
mkMeta Item     = ItemMeta

seed :: Witness b -> Id -> Tag b -> Entity b
seed wit uid s =
    Entity
        {components = Components
            {base = Base.fromTag s
            ,building = Building.fromTag s
            }
        ,meta = mkMeta wit
        ,entityTag = s
        ,uid = uid
        }

mkWorld :: World
mkWorld = World
    {objects    = []
    ,items      = []
    ,lastUid    = 0
    ,initParam  = 0
    ,messages   = []
    ,dirtyTiles = []
}

_entities :: Witness b -> Lens' World [Entity b]
_entities Object = _objects
_entities Item = _items

env :: Env World Entity
env = Env {..} where

    dirtify :: Witness b -> Entity b -> World -> World
    dirtify wit entity = case wit of
                Object -> _dirtyTiles %~ (entity ^. _position :)
                _      -> id

    create :: Witness b -> Tag b -> (Entity b -> Entity b) -> Action World
    create wit s fn
        = Action $ \w@World{lastUid} ->
            let
              entity = fn . seed wit lastUid $ s
            in
                (_entities wit %~ (++ [entity]))
              . (_lastUid +~ 1)
              . dirtify wit entity
              $ w

    withAll :: Witness b -> ([Entity b] -> Action World) -> Action World
    withAll Object act = Action $ \w -> runAction (act (objects w)) w
    withAll Item   act = Action $ \w -> runAction (act (items w)) w

    modify :: Witness b -> (Entity b -> Entity b) -> Entity b -> Action World
    modify wit fn e = Action
                    $ dirtify wit e
                    . dirtify wit (fn e)
                    . (_entities wit . each . filtered (`is` e) %~ fn)

    destroy :: Witness b -> Entity b -> Action World
    destroy wit e = Action
                  $ dirtify wit e
                  . (_entities wit %~ filter (not . (`is` e)))

    is :: Entity b -> Entity b -> Bool
    is = (==) `on` uid

    withInitParam :: (Int -> Action World) -> Action World
    withInitParam act = Action $ \w -> runAction (act (initParam w)) w

    message :: ([String] -> [String]) -> Action World
    message = Action <$> (_messages %~)

    _position :: Lens' (Entity Object) (Int, Int)
    _position = _meta . __position

    _symbol :: Lens' (Entity Object) Char
    _symbol = _meta . __symbol

    _zIndex :: Lens' (Entity Object) Int
    _zIndex = _meta . __zIndex

    _tag :: Lens' (Entity b) (Tag b)
    _tag = _entityTag

feature :: Feature World
feature =  Base.feature env (_components._base)
        <> Building.feature env (_components._building)

engine :: Engine
engine = Engine{..} where

    lastMessage :: EngineM String
    lastMessage = (\x -> if any ('\n' `elem`) x
                    then intercalate "" x
                    else intercalate "; " x)
                .   messages
                <$> St.get

    allSprites :: EngineM [Sprite]
    allSprites = map toSprite
               . sortBy (compare `on` (^. _meta . __zIndex))
               . objects
               <$> St.get
        where toSprite =
                     (,)
                     <$> view (_meta . __position)
                     <*> view (_meta . __symbol)

    changedSprites :: EngineM [Sprite]
    changedSprites = do
            result <- (++) <$> blanks <*> sprites
            St.modify $ _dirtyTiles .~ []
            return result
        where
            blanks   = map (,' ')
                     . dirtyTiles
                     <$> St.get
            sprites  = map toSprite
                     . sortBy (compare `on` (^. _meta . __zIndex))
                     . dirtySprites
                     <$> St.get
            dirtySprites World{dirtyTiles, objects}
                     = filter (view (_meta . __position . to (`elem` dirtyTiles)))
                       objects
            toSprite =
                     (,)
                     <$> view (_meta . __position)
                     <*> view (_meta . __symbol)

    init :: EngineM ()
    init = St.modify . runAction $ Ftr.init feature

    command :: String -> EngineM ()
    command = St.modify . runAction . Ftr.command feature

    step :: EngineM ()
    step = St.modify . runAction $ Ftr.step feature
