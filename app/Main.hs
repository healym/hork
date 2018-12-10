module Main where

import Rooms
import Game
import Commands
import System.IO

play :: State -> IO ()
play state = do
    putStr "\n> "
    hFlush stdout
    input <- getLine
    let (state', mesg) = parseInput state input
    putStrLn mesg
    play state'

main :: IO ()
main = play state
