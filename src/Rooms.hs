module Rooms where

import Data.Char

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
  | Up
  | Down
  deriving (Eq, Show)

stringToDir :: String -> Maybe Direction
stringToDir s =
  case (map toUpper s) of
    "NORTH" -> Just North
    "NORTHEAST" -> Just NorthEast
    "EAST" -> Just East
    "SOUTHEAST" -> Just SouthEast
    "SOUTH" -> Just South
    "SOUTHWEST" -> Just SouthWest
    "WEST" -> Just West
    "NORTHWEST" -> Just NorthWest
    "UP" -> Just Up
    "DOWN" -> Just Down
    _ -> Nothing

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
    MkItem { getUse :: Maybe ( State -> (State, String) )
           , getTake :: Bool
           , getPut :: Bool
           , getDisplay :: String
           }
