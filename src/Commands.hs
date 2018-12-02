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
  | Inventory
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
        "MOVE"      -> Just Move
        "LOOK"      -> Just Look
        "PUT"       -> Just Put
        "TAKE"      -> Just Take
        "USE"       -> Just Use
        "INVENTORY" -> Just Inventory
        "QUIT"      -> Just Quit
        _           -> Nothing

parseLine :: String -> Command
parseLine raw =
  MkCommand { getAction = parseAction . head $ words raw
            , getObject = tail $ words raw
            }

take :: State -> RoomName -> ItemName -> State
take state rname iname =
  case getTake $ fromJust $ lookup iname items of
    False -> state
    True -> case rname == (getLocation $ getPlayer state) of
                True -> takeItem state rname iname
                _     -> state

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


move' :: Player -> Direction -> Maybe Player
move' player direction =
  do
    currentRoom <- lookup (getLocation player) rooms
    nextLocation <- lookup direction $ getExits currentRoom
    Just MkPlayer { getInventory = getInventory player
                  , getLocation = nextLocation
                  , getHealth = getHealth player
                  , getScore = getScore player
                  }

move :: Player -> Direction -> Player
move p d =
  let p' = move' p d in
    case p' of
      Just p'' -> p''
      Nothing  -> p

look :: Player -> State -> String
look player state =
  let
    room = lookup (getLocation player) rooms
    desc = fromJust $ getDescription <$> room
    exits = fromJust $ getExits <$> room
    itemString = getItemDescriptions player state
  in
    parseDescription (desc, exits) ++ itemString

getItemDescriptions :: Player -> State -> String
getItemDescriptions player state =
  let
    lst = (lookup (getLocation player) (getItemLocations state))
  in
    "Here you see a " ++ (listItems $ fromJust $ lst)

listItems :: [ItemName] -> String
listItems [] = ""
listItems (i : []) = i
listItems (i : (i': [])) = i ++ ", and " ++ i'
listItems (i : is) = i ++ ", " ++ listItems is

parseDescription :: (Description, [(Direction, RoomName)]) -> String
parseDescription (Description desc, dirs) =
  let
    dirString = parseDescription' dirs
  in
    desc ++ "\n" ++ (if dirString == "" then "There are no exits." else dirString)

parseDescription' :: [(Direction, RoomName)] -> String
parseDescription' [] = ""
parseDescription' ((dir, _) : dirs) =
  "There is an exit " ++ dirString ++ ".\n" ++ parseDescription' dirs
  where dirString =
          case dir of
            North     -> "to the North"
            NorthEast -> "to the NorthEast"
            East      -> "to the East"
            SouthEast -> "to the SouthEast"
            South     -> "to the South"
            SouthWest -> "to the SouthWest"
            West      -> "to the West"
            NorthWest -> "to the NorthWest"
            Up        -> "upward"
            Down      -> "downward"

showInventory :: Player -> String
showInventory player = (++) "You are carrying:\n" $ showInventory' $ getInventory player

showInventory' :: [ItemName] -> String
showInventory' [] = ""
showInventory' (i : []) = i
showInventory' (i : (i' : [])) = "and " ++ i ++ "\n"
showInventory' (i : is) = i ++ ",\n" ++ showInventory' is