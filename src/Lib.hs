module Lib where

import Data.List

{- Functions:
      Use -- defined per-item
      Take
      Put
      Go
      Look
-}

newtype Description = Description String
newtype RoomName = RoomName String
newtype ItemName = Itemname String
newtype Inventory = Inventory [Item]
type Rooms = [(RoomName, Room)]
type World = (Rooms, Player)

data Player =
  MkPlayer { getInventory :: Inventory
           , getLocation :: RoomName
           , getHealth :: Int
           } deriving (Eq, Show)

data Item =
  MkItem { getName :: ItemName
         , getUse :: World -> World
         , getTake :: Maybe ( Rooms -> RoomName -> Player -> Item -> World )
         , getPut :: Maybe ( Rooms -> RoomName -> Player -> Item -> World )
         } deriving (Eq, Show)

data Room =
  MkRoom { getDescription :: Description
         , getName :: RoomName
         , northExit :: Maybe RoomName
         , eastExit :: Maybe RoomName
         , westExit :: Maybe RoomName
         , southExit :: Maybe RoomName
         , northeastExit :: Maybe RoomName
         , northwestExit :: Maybe RoomName
         , southeastExit :: Maybe RoomName
         , southwestExit :: Maybe RoomName
         , getItems :: [Item]
         , getUnits :: [Unit]
         } deriving (Eq, Show)

look :: Rooms -> RoomName -> Maybe Description
look rs n = getDescription <$> lookup n rs

move :: Player -> Maybe RoomName -> Player
move p Nothing = p
move p Just name =
  MkPlayer { getInventory = getInventory p
           , getLocation = name
           , getHealth = getHealth p
           }

put :: Rooms -> RoomName -> Player -> Item -> World
put rs n p i =
  ((addItemToRoom i n rs), (removeItemFromPlayer i p))

take :: Rooms -> RoomName -> Player -> Item -> World
take rs n p i =
  ((removeItemFromRoom i n rs), (addItemToPlayer i p))

removeItemFromPlayer :: Item -> Player -> Player
removeItemFromPlayer item player =
  MkPlayer { getInventory = [ i | i /= item, i <- getInventory player]
           , getLocation  = getLocation p
           , getHealth    = getHealth p
           }

addItemToPlayer :: Item -> Player -> Player
addItem i p =
  let inventory' = (++) i $ getInventory p in
    MkPlayer { getInventory = inventory'
             , getLocation  = getLocation p
             , getHealth    = getHealth p
             }

removeItemFromRoom :: Rooms -> Item -> RoomName -> Rooms
removeItemFromRoom rs i n =
  let
    room = lookup n rs
    room' = removeItemFromRoom' i <$> room
  in
    case room' of
      Nothing -> rs
      Just r  -> case room of
                   Nothing -> rs
                   Just r' -> (++) (n, r) $ delete (n, r') rs

removeItemFromRoom' :: Item -> Room -> Room
removeItemFromRoom' i room =
MkRoom { getItems = [ i' | i' /= i, i'<-(getItems r)]
       , getDescription = getDescription room
       , getName = getName room
       , northExit = northExit room
       , eastExit = eastExit room
       , westExit = westExit room
       , southExit = southExit room
       , northeastExit = northeastExit room
       , northwestExit = northwestExit room
       , southeastExit = southeastExit room
       , southwestExit = southwestExit room
       , getUnits = getUnits room
       }

addItemToRoom :: Rooms -> Item -> RoomName -> Rooms
addItemToRoom rs i n =
  let
    room = lookup n rs
    room' = addItemToRoom' i <$> room
  in
    case room' of
      Nothing -> rs
      Just r -> case room of
                  Nothing -> rs
                  Just r' -> (++) (n, r) $ delete (n, r') rs

addItemToRoom' :: Item -> Room -> Room
addItemToRoom' item room =
  MkRoom { getItems = (++) item $ getItems room
        , getDescription = getDescription room
        , getName = getName room
        , northExit = northExit room
        , eastExit = eastExit room
        , westExit = westExit room
        , southExit = southExit room
        , northeastExit = northeastExit room
        , northwestExit = northwestExit room
        , southeastExit = southeastExit room
        , southwestExit = southwestExit room
        , getUnits = getUnits room
        }