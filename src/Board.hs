{-# LANGUAGE InstanceSigs #-}

module Board (
  rows,
  columns,
  Color (Empty, Red, Yellow, Both),
  Board (Board),
  Column,
  initialBoard,
  possibleMoves,
  makeMove,
  checkWin,
  valuation,
  opp
  ) where

import Data.List (intercalate, transpose)
import qualified Data.Map.Strict as Map
import Data.Map.Strict((!))

import AI

rows = 6
columns = 7

type Column = Int
type Row = Int
type Coords = (Column, Row)

-- Enumeration for the 3 Colors
data Color = Empty | Red | Yellow | Both deriving (Eq, Show)

-- Board is just a 2 Dimensional List with heights
data Board = Board (Map.Map Coords Color) (Map.Map Column Row) Color Color

initialBoard  = Board (Map.fromList [ ((x, y), Empty) | x <- [1..columns], y <- [1..rows]]) (Map.fromList [ (col, 0) | col <- [1..columns]]) Red Empty

-- Returns Columns whose topmost row is still not filled
possibleMoves :: Board -> [Column]
possibleMoves b@(Board _ heights _ _)
  | checkWin b = []
  | otherwise = [x | x <- [1..columns], (heights ! x) /= rows]

-- Update the Board
makeMove :: Board -> Column -> Board
makeMove b@(Board board heights color k) col = let posMoves = possibleMoves b
                                                   curHeight = heights ! col
                                                   nBoard = Map.insert (col, curHeight + 1) color board
                                                   nHeights = Map.insert col (curHeight + 1) heights
                                               in if (col `elem` posMoves) then Board nBoard nHeights (opp color) k else b

opp :: Color -> Color
opp c
  | c == Red = Yellow
  | c == Yellow = Red


verticals :: Board -> [[Color]]
verticals (Board board _ _ _) = [ [board ! (x, y + i) | i <- [0..3]] | x <- [1..columns], y <- [1..(rows - 3)]]

horizontals :: Board -> [[Color]]
horizontals (Board board _ _ _) = [ [board ! (x + i, y) | i <- [0..3]] | x <- [1..(columns - 3)], y <- [1..rows]]

rdiags :: Board -> [[Color]]
rdiags (Board board _ _ _) = [ [board ! (x + i, y + i) | i <- [0..3]] | x <- [1..(columns - 3)], y <- [1..(rows - 3)]]

ldiags :: Board -> [[Color]]
ldiags (Board board _ _ _) = [ [board ! (x + i, y - i) | i <- [0..3]] | x <- [1..(columns - 3)], y <- [4..rows]]

-- Check if a player of particular color has won
checkWin :: Board -> Bool
checkWin board = or [f board | f <- [checkWinCol, checkWinRow, checkWinDiagRight, checkWinDiagLeft]]

-- check if any quad exists such that all elements of a quad are of the opposite color.
-- If yes then the other player won and reached this state
checkWin' :: (Board -> [[Color]]) -> Board -> Bool
checkWin' f b@(Board board _ color _) = or $ map (and . map ((==) (opp color))) $ f b

checkWinCol :: Board -> Bool
checkWinCol = checkWin' verticals

checkWinRow :: Board -> Bool
checkWinRow = checkWin' horizontals

checkWinDiagRight :: Board -> Bool
checkWinDiagRight = checkWin' rdiags

checkWinDiagLeft :: Board -> Bool
checkWinDiagLeft = checkWin' ldiags

-- Calculate value of a particular board w.r.t to Color
valuation :: Board -> Int
valuation board = sum [f board | f <- [valuationCol, valuationRow, valuationDiagRight, valuationDiagLeft]]

-- Calculate score of each quad set and return the sum
valuation' :: (Board -> [[Color]]) -> Board -> Int
valuation' f b@(Board board _ color _) = sum $ map (scoreQuad color) $ f b

valuationCol :: Board -> Int
valuationCol = valuation' verticals

valuationRow :: Board -> Int
valuationRow = valuation' horizontals

valuationDiagRight :: Board -> Int
valuationDiagRight = valuation' rdiags

valuationDiagLeft :: Board -> Int
valuationDiagLeft = valuation' ldiags

scoreQuad :: Color -> [Color] -> Int
scoreQuad color colors = if (and [ c == Empty || c == color | c <- colors])
                         then case (length $ filter (== color) colors) of 3 -> 1
                                                                          _ -> 0
                         else 0

-- Generate all possible next move positions
-- Uses possibleMoves and makeMove
-- moves :: Board -> Color -> [Board]

instance GamePosition Board where
  moves :: Board -> [Board]
  moves b = map (makeMove b) $ possibleMoves b

  static :: Board -> Int
  static b@(Board board h c k) = if (checkWin b) then -100 else (valuation b) - (valuation (Board board h (opp c) k))

instance Show Board where
  show :: Board -> String
  show (Board b _ _ _) = let strBoard = [[if z == Red then "R" else if z == Yellow then "Y" else " " | x <- [1..columns], let z = b ! (x, y)] | y <- [rows,(rows-1)..1]]
                             rowSep = "\n" ++ (concat $ replicate columns "+---") ++ "+\n"
                             output = intercalate rowSep $ map (\x ->"| " ++ (intercalate " | " x) ++ " |") strBoard
                         in rowSep ++ output ++ rowSep
