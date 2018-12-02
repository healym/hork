module Commands where


import Data.Char
import Data.Maybe
import Rooms
import Game

data Action =
    Move
  | Look
  | Put
  | Take
  | Use
  | Quit
  deriving (Eq, Show)

type Object = [String]

data Command =
  MkCommand { getAction::Maybe Action
            , getObject::Object
            }

parseAction :: String -> Maybe Action
parseAction actionString =
    case ( map toUpper actionString ) of
        "MOVE" -> Just Move
        "LOOK" -> Just Look
        "PUT"  -> Just Put
        "TAKE" -> Just Take
        "USE"  -> Just Use
        "QUIT" -> Just Quit
        _      -> Nothing

parseLine :: String -> Command
parseLine raw =
  MkCommand { getAction = parseAction . head $ words raw
            , getObject = tail $ words raw
            }

takeItem :: State -> RoomName -> ItemName -> State
takeItem state rname item =
  let
    player = getPlayer state
    item' = if elem item $ fromJust $ lookup rname $ getItemLocations state then [item] else []
    player' = MkPlayer { getInventory = (++) item' $ getInventory player
                       , getLocation = getLocation player
                       , getHealth = getHealth player
                       , getScore = getScore player
                       }
    itemLocList = fromJust $ lookup rname $ getItemLocations state
    itemLocList' = [ i | i <- itemLocList, i /= item ]
    itemLocPair = (rname, itemLocList')
    itemLoc = [ p | p <- getItemLocations state, p /= (rname, itemLocList) ] ++ [itemLocPair]
  in
    MkState { getItemLocations = itemLoc
            , getPlayer = player'
            }

putItem :: State -> RoomName -> ItemName -> State
putItem state rname item =
  let
    player = getPlayer state
    item' = if elem item (getInventory player) then [item] else []
    itemLocList = fromJust $ lookup rname $ getItemLocations state
    itemLocList' = itemLocList ++ item'
    itemLocPair = (rname, itemLocList')
    itemLoc = [ p | p <- getItemLocations state, p /= (rname, itemLocList) ] ++ [itemLocPair]
  in
    MkState { getItemLocations = itemLoc
            , getPlayer = getPlayer state
            }


move :: Player -> Direction -> Player
move player direction =
  do
    currentLocation <- getLocation player
    currentRoom <- lookup currentLocations rooms
    adjRooms <- getExits currentRoom
    nextLocation <- lookup direction adjRooms
    MkPlayer { getInventory = getInventory player
             , getLocation = newRoomName
             , getHealth = getHealth player
             , getScore = getScore player
             }