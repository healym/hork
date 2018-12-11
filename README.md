# Hork
Hork is a text-adventure game platform written in the Haskell programming
language. This was done as a learning exercise to better understand Haskell,
but ended up being a good idea for a couple of reasons.

1. Defining a new game within Hork is trivial.
2. Defining the `Use` command per-item is fairly intuitive, and is made easy
by haskell's `Maybe` monad and its ability to wrap functions.
3. Haskell is a very good language for developing domain-specific languages
(DSLs), which points to a good future of this project (becoming a domain
specific language for developing text-based adventure games)

## Building the project
In order to use hork, you need to have installed `stack`, instructions for
installation can be found
[here](https://docs.haskellstack.org/en/stable/README/#how-to-install).

Once you have stack installed, and have cloned the repository for hork,
run either `stack run` or `stack exec hork` from within the repository,
depending on your stack installation.

## Tutorial
### Playing Hork
In order to play Hork, there are 6 commands you should understand.

- `use <item>`: This allows you to use an item. If the item in question does not
have a defined use, the game will inform you that you do not know how to use the
item.
- `move <direction>`: This allows you to move in any of the 8 directions on a
compass, as well as upward or downward.
- `look`: This allows you to look at your surroundings, and will output a text
description of your current location into your terminal. This is done automatically
any time you move to a new location.
- `take <item>`: This allows you to pick up an item in the same location as you
and place it in your inventory.
- `put <item>`: This allows you to place an item from your inventory into your
current location
- `help`: This will display a help menu reiterating what you have just read.

### Creating Your Own Game
Hork has a trivial non-game already programmed in, in the file `src/Game.hs`.
In order to substiture your own game, write your own Room and Item types in
your own file, and import that file in `app/Main.hs`, in place of `Game`.

#### The `Room` type
Rooms have 3 fields, a `Description`, a `RoomName`, and a list of exits
(stored as `Direction`s). You can define a new room as follows:
```haskell
demo = RoomName "Demo-Room"
demoRoom = MkRoom { getDescription = "You are in a white room. It is completely nondescript, as if for a demo."
                  , getRoomName = demo
                  , getExits = [ (North, demo)
                               , (South, demo)
                               ]
                  }
```

By doing this, we have defined a demo room. The description, as will be output
by the `look` command, is "You are in a white room. It is completely
nondescript, as if for a demo." It has only two exits, to the north and south,
both of which loop back into the room. You may notice that the roomname is
stored in a constant. This is recommended, as it prevents the mistyping of
strings when adding the room to the master rooms association list. Let's do that
now, so that you can see why the constant might be helpful.
```haskell
rooms = [(demo, demoRoom)]
```
Now we have exactly one room in our game world: this demo room. This `rooms`
association list is what is referenced by all movement to prevent rooms being
recursive data structures (which is what would happen if rooms contained other
rooms.)

Lets add some items to see how they're handled in hork.
```haskell
buttonName = "button"
button = MkItem { getUse = Just buttonUse
                 , getPut = True
                 , getTake = True
                 , getDisplay = "an enticing red button, ready to be pushed."
                 }

buttonUse :: State -> (State, String)
buttonUse state = (state, "Click!")

items = [ (buttonName, button) ]
```
Now we've created a nice, enticing red button, that clicks when you press it.
We store the item itself in an association list, and pass around the name of the
item as a key to that list.

Let's add it to the demo room:
```haskell
itemLocations = [(buttonName, demo)]
```
It's really that easy!

Now we have a demo room with a button in it. This is a proof of concept, but you
can build an entire game with these rooms and buttons, no other work required!