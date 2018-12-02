module Game where

import Rooms

bin = MkItem { getUse = Nothing
             , getPut = True
             , getTake = False
             }


whiskey = MkItem { getUse = Nothing
                 , getPut = True
                 , getTake = False
                 }

items = [ ("Bin", bin)
        , ("Whiskey", whiskey)
        ]

kitchen = RoomName "Kitchen"
den = RoomName "Den"
yard = RoomName "Yard"

denRoom :: Room
denRoom = MkRoom { getDescription = Description "living room desc"
                 , getRoomName = den
                 , getExits = [(South, kitchen)]
                 }

kitchenRoom :: Room
kitchenRoom = MkRoom { getDescription = Description "kitchen"
                     , getRoomName = kitchen
                     , getExits = [ (North, den)
                                  , (West, yard)
                                  ]
                     }

yardRoom :: Room
yardRoom = MkRoom { getDescription = Description "yard"
         , getRoomName = yard
         , getExits = [(East, kitchen)]
         }

rooms = [ (yard, yardRoom)
        , (kitchen, kitchenRoom)
        , (den, denRoom)
        ]