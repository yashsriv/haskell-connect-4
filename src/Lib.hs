module Lib
    ( play
    ) where

import Data.List (intercalate, transpose)
import qualified Data.Map.Strict as Map
import Data.Map.Strict((!))

import Board (
  Board (Board), Color(Red, Yellow), Column,
  possibleMoves, makeMove, rows, columns, checkWin, valuation
    )

play :: Board -> Color -> IO ()
play b color =
  do let moves = possibleMoves b
     displayBoard b
     putStrLn $ "Valuation of Red: " ++ (show $ valuation b Red)
     putStrLn $ "Valuation of Yellow: " ++ (show $ valuation b Yellow)
     putStr "Possible Moves: "
     putStrLn $ unwords $ map show moves
     moveString <- getLine
     let move = (read moveString) :: Int
         func = if (move `elem` moves) then performMove else invalidMove
     func b color move

performMove :: Board -> Color -> Column -> IO ()
performMove b color col =
  do let nBoard = makeMove b color col
         nColor = if color == Red then Yellow else Red
         hasWon = checkWin nBoard color
     if hasWon then showVictory nBoard color else play nBoard nColor

invalidMove :: Board -> Color -> Column -> IO ()
invalidMove b color c =
  do putStrLn $ "You have entered an invalid move: " ++ (show c)
     putStrLn "Please choose one of the possible moves."
     play b color

displayBoard :: Board -> IO ()
displayBoard (Board b _) = let strBoard = [[if z == Red then "1" else if z == Yellow then "2" else " " | x <- [1..columns], let z = b ! (x, y)] | y <- [rows,(rows-1)..1]]
                               rowSep = "\n" ++ (concat $ replicate columns "+---") ++ "+\n"
                               output = intercalate rowSep $ map (\x ->"| " ++ (intercalate " | " x) ++ " |") strBoard
                           in putStrLn $ rowSep ++ output ++ rowSep

showVictory :: Board -> Color -> IO ()
showVictory b c = do displayBoard b
                     putStrLn ((show c) ++ " has won...")
