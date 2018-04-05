module Main where

import Board (initialBoard)
import Lib (play)

main :: IO ()
main = play initialBoard
