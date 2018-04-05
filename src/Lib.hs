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

import AI

play :: Board -> IO ()
play b@(Board board h _) =
  do let moves = possibleMoves b
     displayBoard b
     -- putStrLn $ "Evaluate: " ++ (show $ evaluate 5 b)
     putStrLn $ "Valuation of Red: " ++ (show $ valuation (Board board h Yellow))
     putStrLn $ "Valuation of Yellow: " ++ (show $ valuation (Board board h Red))
     putStr "Possible Moves: "
     putStrLn $ unwords $ map show moves
     moveString <- getLine
     let move = (read moveString) :: Int
         func = if (move `elem` moves) then performMove else invalidMove
     func b move

performMove :: Board -> Column -> IO ()
performMove b col =
  do let nBoard = makeMove b col
         hasWon = checkWin nBoard
     if hasWon then showVictory nBoard else play nBoard

invalidMove :: Board -> Column -> IO ()
invalidMove b c =
  do putStrLn $ "You have entered an invalid move: " ++ (show c)
     putStrLn "Please choose one of the possible moves."
     play b

displayBoard :: Board -> IO ()
displayBoard (Board b _ _) = let strBoard = [[if z == Red then "R" else if z == Yellow then "Y" else " " | x <- [1..columns], let z = b ! (x, y)] | y <- [rows,(rows-1)..1]]
                                 rowSep = "\n" ++ (concat $ replicate columns "+---") ++ "+\n"
                                 output = intercalate rowSep $ map (\x ->"| " ++ (intercalate " | " x) ++ " |") strBoard
                             in putStrLn $ rowSep ++ output ++ rowSep

showVictory :: Board -> IO ()
showVictory b@(Board _ _ c) = do displayBoard b
                                 putStrLn ((show c) ++ " has lost...")
