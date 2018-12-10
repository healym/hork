module Game where

import Rooms

bucketName = "bucket"
bucket = MkItem { getUse = Nothing
                , getPut = True
                , getTake = True
                , getDisplay = "a bucket"
                }

whiskeyUse :: State -> (State, String)
whiskeyUse state =
    let
        score = getScore $ getPlayer state
        player = getPlayer state
        inventory = getInventory player
        inventory' = [ i | i <- inventory, i /= whiskeyName ] ++ [ emptyWhiskeyName ]
        player' = MkPlayer { getInventory = inventory'
                           , getLocation = getLocation player
                           , getHealth = (getHealth player) - 15
                           , getScore = (getScore player) + 50
                           }
        state' = MkState { getPlayer = player'
                         , getItemLocations = getItemLocations state
                         }
        mesg = "...GLUG GLUG GLUG GLUG...\n"
            ++ "\n Aaah... Oddly refreshing."
    in
        (state', mesg)


whiskeyName = "whiskey"
whiskey = MkItem { getUse = Just whiskeyUse
                 , getPut = True
                 , getTake = True
                 , getDisplay = "a bottle of whiskey"
                 }

emptyWhiskeyName = "bottle"
emptyWhiskey = MkItem { getUse = Nothing
                      , getPut = True
                      , getTake = True
                      , getDisplay = "an empty bottle with a label reading 'WHISKEY'"
                      }

frogName = "frog"
frog = MkItem { getUse = Nothing
              , getPut = True
              , getTake = True
              , getDisplay = "a frog"
              }

chainName = "chain"
chain = MkItem { getUse = Nothing
               , getPut = True
               , getTake = True
               , getDisplay = "a chain"
               }

items = [ (bucketName, bucket)
        , (whiskeyName, whiskey)
        , (frogName, frog)
        , (chainName, chain)
        , (emptyWhiskeyName, emptyWhiskey)
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