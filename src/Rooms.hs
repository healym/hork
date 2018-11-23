module Rooms where

import Lib

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