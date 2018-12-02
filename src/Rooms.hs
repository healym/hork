module Rooms where

newtype Description = Description String deriving (Eq, Show)

newtype RoomName = RoomName String deriving (Eq, Show)

type ItemLocations = [(RoomName, [ItemName])]

data Direction =
    North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest
  deriving (Eq, Show)

type Rooms = [(RoomName, Room)]

data State =
  MkState { getItemLocations :: ItemLocations
          , getPlayer :: Player
          }

data Player =
  MkPlayer { getInventory :: Inventory
           , getLocation :: RoomName
           , getHealth :: Int
           , getScore :: Int
           }

data Room =
  MkRoom { getDescription :: Description
         , getRoomName :: RoomName
         , getExits :: [(Direction, RoomName)]
         }


type ItemName = String

type Inventory = [ItemName]

data Item =
    MkItem { getUse :: Maybe ( State -> State )
           , getTake :: Bool
           , getPut :: Bool
           }
