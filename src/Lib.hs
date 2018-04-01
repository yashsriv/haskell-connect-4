module Lib
    ( play
    ) where

import Data.List (intercalate, transpose)

import Board (
  Board (Board), Color(Red, Yellow), Column,
  possibleMoves, makeMove, maxCol
    )

play :: Board -> Color -> IO ()
play b color =
  do let moves = possibleMoves b
     displayBoard b
     putStr "Possible Moves: "
     putStrLn $ unwords $ map show moves
     moveString <- getLine
     let move = (read moveString) :: Int
         func = if (move `elem` moves) then performMove else invalidMove
     func b color move

performMove :: Board -> Color -> Column -> IO ()
performMove b color col =
  do let nboard = makeMove b color col
         ncolor = if color == Red then Yellow else Red
     -- Check for win condition
     play nboard ncolor

invalidMove :: Board -> Color -> Column -> IO ()
invalidMove b color c =
  do putStrLn $ "You have entered an invalid move: " ++ (show c)
     putStrLn "Please choose one of the possible moves."
     play b color

displayBoard :: Board -> IO ()
displayBoard (Board b _) = let strBoard = map (map (\x -> if x == Red then "1" else if x == Yellow then "2" else " ")) $ reverse $ transpose b
                               rowSep = "\n" ++ (concat $ replicate maxCol "+---") ++ "+\n"
                               output = intercalate rowSep $ map (\x ->"| " ++ (intercalate " | " x) ++ " |") strBoard
                           in putStrLn $ rowSep ++ output ++ rowSep
