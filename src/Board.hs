module Board (
  rows,
  columns,
  Color (Red, Yellow),
  Board (Board),
  Column,
  initialBoard,
  possibleMoves,
  makeMove,
  checkWin,
  valuation,
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict((!))

import AI

rows = 6
columns = 7

type Column = Int
type Row = Int
type Coords = (Column, Row)

-- Enumeration for the 3 Colors
data Color = Empty | Red | Yellow deriving (Eq, Show)

-- Board is just a 2 Dimensional List with heights
data Board = Board (Map.Map Coords Color) (Map.Map Column Row) Color

initialBoard  = Board (Map.fromList [ ((x, y), Empty) | x <- [1..columns], y <- [1..rows]]) (Map.fromList [ (col, 0) | col <- [1..columns]]) Red

-- Returns Columns whose topmost row is still not filled
possibleMoves :: Board -> [Column]
possibleMoves (Board _ heights _)= [x | x <- [1..columns], (heights ! x) /= rows]

-- Update the Board
makeMove :: Board -> Column -> Board
makeMove (Board board heights color) col = let curHeight = heights ! col
                                               nBoard = Map.insert (col, curHeight + 1) color board
                                               nHeights = Map.insert col (curHeight + 1) heights
                                           in Board nBoard nHeights (opp color)

opp :: Color -> Color
opp c
  | c == Red = Yellow
  | c == Yellow = Red

-- Check if a player of particular color has won
checkWin :: Board -> Bool
checkWin board = or [f board | f <- [checkWinCol, checkWinRow, checkWinDiagRight, checkWinDiagLeft]]

checkWinCol :: Board -> Bool
checkWinCol (Board board _ color) = or [and [(board ! (x, y + i)) == (opp color) | i <- [0..3]] | x <- [1..columns], y <- [1..(rows - 3)]]

checkWinRow :: Board -> Bool
checkWinRow (Board board _ color) = or [and [(board ! (x + i, y)) == (opp color) | i <- [0..3]] | y <- [1..rows], x <- [1..(columns - 3)]]

checkWinDiagRight :: Board -> Bool
checkWinDiagRight (Board board _ color) = or [and [(board ! (x + i, y + i)) == (opp color) | i <- [0..3]] | y <- [1..(rows - 3)], x <- [1..(columns - 3)]]

checkWinDiagLeft :: Board -> Bool
checkWinDiagLeft (Board board _ color) = or [and [(board ! (x + i, y - i)) == (opp color) | i <- [0..3]] | y <- [4..rows], x <- [1..(columns-3)]]

-- Calculate value of a particular board w.r.t to Color
valuation :: Board -> Int
valuation board = sum [f board | f <- [valuationCol, valuationRow, valuationDiagRight, valuationDiagLeft]]

valuationCol :: Board -> Int
valuationCol (Board board _ color) = sum [ scoreQuad color [board ! (x, y + i) | i <- [0..3]] | x <- [1..columns], y <- [1..(rows - 3)]]

valuationRow :: Board -> Int
valuationRow (Board board _ color) = sum [scoreQuad color [board ! (x + i, y) | i <- [0..3]] | y <- [1..rows], x <- [1..(columns - 3)]]

valuationDiagRight :: Board -> Int
valuationDiagRight (Board board _ color) = sum [scoreQuad color [board ! (x + i, y + i) | i <- [0..3]] | y <- [1..(rows - 3)], x <- [1..(columns - 3)]]

valuationDiagLeft :: Board -> Int
valuationDiagLeft (Board board _ color) = sum [scoreQuad color [board ! (x + i, y - i) | i <- [0..3]] | y <- [4..rows], x <- [1..(columns - 3)]]

scoreQuad :: Color -> [Color] -> Int
scoreQuad color colors = if (and [ c == Empty || c == color | c <- colors])
                         then if (length $ filter (== color) colors) == 3 then 1 else 0
                         else 0

-- Generate all possible next move positions
-- Uses possibleMoves and makeMove
-- moves :: Board -> Color -> [Board]

instance GamePosition Board where
  -- moves :: Board -> [Board]
  moves b = map (makeMove b) $ possibleMoves b
  static b@(Board board h c) = if (checkWin b) then 100 else (valuation b) - (valuation (Board board h (opp c)))
