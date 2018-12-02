module Commands where


import Data.Char
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

take :: State -> RoomName -> Item -> State
take = undefined

put :: State -> RoomName -> Item -> State
put = undefined

getItemFromInventory :: Player -> ItemName -> Maybe Item
getItemFromInventory player name =
  case elem name (getInventory player) of
    True -> lookup name items
    False -> Nothing