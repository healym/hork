module Game where

import Rooms

bucketName = "a bucket"
bucket = MkItem { getUse = Nothing
                , getPut = True
                , getTake = True
                }

whiskeyName = "a bottle of whiskey"
whiskey = MkItem { getUse = Nothing
                 , getPut = True
                 , getTake = True
                 }

frogName = "a frog"
frog = MkItem { getUse = Nothing
              , getPut = True
              , getTake = True
              }

chainName = "a chain"
chain = MkItem { getUse = Nothing
               , getPut = True
               , getTake = True
               }

items = [ (bucketName, bucket)
        , (whiskeyName, whiskey)
        , (frogName, frog)
        , (chainName, chain)
        ]

den = RoomName "Living Room"
attic = RoomName "Attic"
garden = RoomName "Garden"

livingRoom :: Room
livingRoom = MkRoom { getDescription = (Description "You are in the living-room. A wizard is snoring loudly on the couch.")
                    , getRoomName = den
                    , getExits = [ (Up, attic)
                                 , (West, garden)
                                 ]
                    }

atticRoom :: Room
atticRoom = MkRoom { getDescription = (Description "You are in the attic. There is a giant welding torch in the corner.")
                     , getRoomName = attic
                     , getExits = [ (Down, den)]
                     }

gardenRoom :: Room
gardenRoom = MkRoom { getDescription = (Description "You are in a beautiful garden. There is a well in front of you.")
                    , getRoomName = garden
                    , getExits = [(East, den)]
                    }

rooms = [ (garden, gardenRoom)
        , (den, livingRoom)
        , (attic, atticRoom)
        ]

player :: Player
player = MkPlayer { getInventory = []
                  , getLocation = den
                  , getHealth = 100
                  , getScore = 0
                  }

state :: State
state = MkState { getItemLocations = [ (garden, [frogName, chainName])
                                     , (den, [whiskeyName, bucketName])
                                     , (attic, [])
                                     ]
                , getPlayer = player
                }