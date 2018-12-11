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
  | Help
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
        "HELP"      -> Just Help
        _           -> Nothing

helpMesg :: String
helpMesg = "AVAILABLE COMMANDS:" ++
           "   COMMAND               EFFECT  \n" ++
           "MOVE <DIRECTION> -> Move in a given direction.\n" ++
           "LOOK             -> Look at your surroundings.\n" ++
           "PUT <ITEM>       -> Drop an item in your inventory.\n" ++
           "TAKE <ITEM>      -> Take an item into your inventory.\n" ++
           "USE <ITEM>       -> Use an item.\n" ++
           "INVENTORY        -> Show your inventory.\n" ++
           "HELP             -> Show this help menu.\n"

parseLine :: String -> Command
parseLine raw =
  MkCommand { getAction = parseAction . head $ words raw
            , getObject = tail $ words raw
            }

parseCommand :: State -> Command -> (State, String)
parseCommand state command =
  case getAction command of
    Nothing   -> (state, "I don't know how to do that.")
    Just Move -> let
                   mDir = stringToDir $ head $ getObject command
                   state' = move state 
                 in
                   case mDir of
                    Nothing -> (state, "I don't know where you want me to go.")
                    Just dir -> let state' = move state dir in (state', (look state'))
    Just Look -> (state, look state)
    Just Take -> let
                   item = head $ getObject command
                   state' = Commands.take state item
                   ploc = getLocation $ getPlayer state
                   ilocs = getItemLocations state
                 in case elem item (fromJust $ lookup ploc ilocs) of
                   True -> (state', "Took the " ++ item ++ ".")
                   False -> (state, "I don't see that.")
    Just Put -> let
                  item = head $ getObject command
                  state' = Commands.put state item
                  inventory = getInventory $ getPlayer state
                in case elem item inventory of
                  True -> (state', "Okay.")
                  False -> (state, "I don't have that.")
    Just Inventory -> (state, (showInventory state))
    Just Use -> let
                  item = head $ getObject command
                  item' = fromJust $ lookup item items
                  inventory = getInventory $ getPlayer state
                in
                  case elem item inventory of
                    False -> (state, "I'd have to have that first.")
                    True -> case getUse item' of
                              Nothing -> (state, "I don't know how to use that")
                              Just f -> f state
    Just Help -> (state, helpMesg)
    _ -> (state, "I'm not sure what you mean.")

parseInput :: State -> String -> (State, String)
parseInput state "" = (state, "")
parseInput state string = (parseCommand state) . parseLine $ string


take :: State -> ItemName -> State
take state iname =
  let
    ploc = getLocation $ getPlayer state
    ilocs = getItemLocations state
  in
    case getTake $ fromJust $ lookup iname items of
      False -> state
      True -> case elem iname (fromJust $ lookup ploc ilocs) of
                  True -> takeItem state ploc iname
                  _     -> state

put :: State -> ItemName -> State
put state iname =
  let
    ploc = getLocation $ getPlayer state
  in
    case getPut $ fromJust $ lookup iname items of
      False -> state
      True -> case elem iname (getInventory $ getPlayer state) of
                False -> state
                True -> putItem state ploc iname

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
    inventory = [ i | i <- getInventory player, i /= item ]
    item' = if elem item (getInventory player) then [item] else []
    itemLocList = fromJust $ lookup rname $ getItemLocations state
    itemLocList' = itemLocList ++ item'
    itemLocPair = (rname, itemLocList')
    itemLoc = [ p | p <- getItemLocations state, p /= (rname, itemLocList) ] ++ [itemLocPair]
    player' = MkPlayer { getInventory = inventory
                       , getLocation = getLocation player
                       , getHealth = getHealth player
                       , getScore = getScore player
                       }
  in
    MkState { getItemLocations = itemLoc
            , getPlayer = player'
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

move :: State -> Direction -> State
move s d =
  let
    p = getPlayer s
    p' = move' p d
  in
    case p' of
      Just p'' -> MkState { getPlayer = p'', getItemLocations = getItemLocations s}
      Nothing  -> s

look :: State -> String
look state =
  let
    player = getPlayer state
    room = lookup (getLocation player) rooms
    desc = fromJust $ getDescription <$> room
    exits = fromJust $ getExits <$> room
    itemString = getItemDescriptions state
  in
    parseDescription (desc, exits) ++ itemString

getItemDescriptions :: State -> String
getItemDescriptions state =
  let
    lst = (lookup (getLocation $ getPlayer state) (getItemLocations state))
  in
    if (length $ fromJust lst) > 0 then
      "Here you see " ++ (listItems $ fromJust $ lst)
    else
      ""

listItems :: [ItemName] -> String
listItems [] = ""
listItems (i : []) = getDisplay $ fromJust $ lookup i items
listItems (i : (i': [])) = (getDisplay $ fromJust $ lookup i items) ++ ", and " ++ (getDisplay $ fromJust $ lookup i' items)
listItems (i : is) = (getDisplay $ fromJust $ lookup i items) ++ ", " ++ listItems is

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

showInventory :: State -> String
showInventory state = ((++) "You are carrying:\n" $ showInventory' $ getInventory $ getPlayer state)
                      ++ "\n\nYour health is: " ++ (show $ getHealth $ getPlayer state)
                      ++ "\nYour score is: " ++ (show $ getScore $ getPlayer state) ++ "\n"

showInventory' :: [ItemName] -> String
showInventory' [] = ""
showInventory' (i : []) = getDisplay $ fromJust $ lookup i items
showInventory' (i : is) = (getDisplay $ fromJust $ lookup i items) ++ ",\n" ++ showInventory' is
