module Main where

import Rooms
import Game
import Commands

play :: State -> IO ()
play state = do
    input <- getLine
    let (state', mesg) = parseInput state input
    putStrLn mesg
    play state'

main :: IO ()
main = play state
