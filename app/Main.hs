module Main where

import Board (initialBoard, Color(Red))
import Lib (play)

main :: IO ()
main = play initialBoard Red
