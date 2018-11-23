module Lib where

import Data.List

{- Functions:
      Use -- defined per-item
      Take
      Put
      Go
      Look
-}

{-
  constants to hold room names
-}
let
  YARD = "Yard"
  KITCHEN = "Kitchen"
  LIVINGROOM = "Living Room"
in

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
  ((addItem i n rs), (removeItem i p))

addItem :: Rooms -> Item -> RoomName -> Rooms
addItem rs i n =
  let
    room = lookup n rs
    room' = addItem' i <$> room
  in
    case room' of
      Nothing -> rs
      Just r -> case room of
                  Nothing -> rs
                  Just r' -> (++) (n, r) $ delete (n, r') rs

addItem' :: Item -> Room -> Room
addItem' item room =
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

removeItem :: Item -> Player -> Player
removeItem item player =
  MkPlayer { getInventory = [ i | i /= item, i <- getInventory player]
           , getLocation  = getLocation p
           , getHealth    = getHealth p
           }

take :: Rooms -> RoomName -> Player -> Item -> World
take rs n p i =
  ((removeItem i n rs), (addItem i p))

addItem :: Item -> Player -> Player
addItem i p =
  let inventory' = (++) i $ getInventory p in
    MkPlayer { getInventory = inventory'
             , getLocation  = getLocation p
             , getHealth    = getHealth p
             }

removeItem :: Rooms -> Item -> RoomName -> Rooms
removeItem rs i n =
  let
    room = lookup n rs
    room' = removeItem' i <$> room
  in
    case room' of
      Nothing -> rs
      Just r  -> case room of
                   Nothing -> rs
                   Just r' -> (++) (n, r) $ delete (n, r') rs

removeItem' :: Item -> Room -> Room
removeItem' i room =
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


{-
    Example Rooms
-}

livingRoom = MkRoom { getDescription = Description "living room desc"
                    , getName = RoomName LIVINGROOM
                    , northExit = Nothing
                    , eastExit = Nothing
                    , westExit = Nothing
                    , southExit = Just KITCHEN
                    , northwestExit = Nothing
                    , northeastExit = Nothing
                    , southwestExit = Nothing
                    , southeastExit = Nothing
                    , getItems = []
                    , getUnits = []
                    }

kitchen = MkRoom { getDescription = Description "kitchen"
                 , getName = RoomName KITCHEN
                 , northExit = Just LIVINGROOM
                 , eastExit = Nothing
                 , westExit = KITCHEN
                 , southExit = Nothing
                 , northwestExit = Nothing
                 , northeastExit = Nothing
                 , southwestExit = Nothing
                 , southeastExit = Nothing
                 , getItems = []
                 , getUnits = []
                 }

yard = MkRoom { getDescription = Description "yard"
              , getName = RoomName YARD
              , northExit = Nothing
              , eastExit = KITCHEN
              , westExit = Nothing
              , southExit = Nothing
              , northwestExit = Nothing
              , northeastExit = Nothing
              , southwestExit = Nothing
              , southeastExit = Nothing
              , getItems = []
              , getUnits = []
              }

