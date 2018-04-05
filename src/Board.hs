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

rows = 6
columns = 7

type Column = Int
type Row = Int
type Coords = (Column, Row)

-- Enumeration for the 3 Colors
data Color = Empty | Red | Yellow deriving (Eq, Show)

-- Board is just a 2 Dimensional List with heights
data Board = Board (Map.Map Coords Color) (Map.Map Column Row)

initialBoard  = Board (Map.fromList [ ((x, y), Empty) | x <- [1..columns], y <- [1..rows]]) (Map.fromList [ (col, 0) | col <- [1..columns]])

-- Returns Columns whose topmost row is still not filled
possibleMoves :: Board -> [Column]
possibleMoves (Board _ heights)= [x | x <- [1..columns], (heights ! x) /= rows]

-- Update the Board
makeMove :: Board -> Color -> Column -> Board
makeMove (Board board heights) color col = let curHeight = heights ! col
                                               nBoard = Map.insert (col, curHeight + 1) color board
                                               nHeights = Map.insert col (curHeight + 1) heights
                                           in Board nBoard nHeights


-- Check if a player of particular color has won
checkWin :: Board -> Color -> Bool
checkWin board color = or [f board color | f <- [checkWinCol, checkWinRow, checkWinDiagRight, checkWinDiagLeft]]

checkWinCol :: Board -> Color -> Bool
checkWinCol (Board board _) color = or [and [(board ! (x, y + i)) == color | i <- [0..3]] | x <- [1..columns], y <- [1..(rows - 4)]]

checkWinRow :: Board -> Color -> Bool
checkWinRow (Board board _) color = or [and [(board ! (x + i, y)) == color | i <- [0..3]] | y <- [1..rows], x <- [1..(columns - 4)]]

checkWinDiagRight :: Board -> Color -> Bool
checkWinDiagRight (Board board _) color = or [and [(board ! (x + i, y + i)) == color | i <- [0..3]] | y <- [1..(rows - 4)], x <- [1..(columns - 4)]]

checkWinDiagLeft :: Board -> Color -> Bool
checkWinDiagLeft (Board board _) color = or [and [(board ! (x - i, y - i)) == color | i <- [0..3]] | y <- [4..rows], x <- [4..columns]]

-- Calculate value of a particular board w.r.t to Color
valuation :: Board -> Color -> Int
valuation board color = sum [f board color | f <- [valuationCol, valuationRow, valuationDiagRight, valuationDiagLeft]]

valuationCol :: Board -> Color -> Int
valuationCol (Board board _) color = sum [ scoreQuad color [board ! (x, y + i) | i <- [0..3]] | x <- [1..columns], y <- [1..(rows - 4)]]

valuationRow :: Board -> Color -> Int
valuationRow (Board board _) color = sum [scoreQuad color [board ! (x + i, y) | i <- [0..3]] | y <- [1..rows], x <- [1..(columns - 4)]]

valuationDiagRight :: Board -> Color -> Int
valuationDiagRight (Board board _) color = sum [scoreQuad color [board ! (x + i, y + i) | i <- [0..3]] | y <- [1..(rows - 4)], x <- [1..(columns - 4)]]

valuationDiagLeft :: Board -> Color -> Int
valuationDiagLeft (Board board _) color = sum [scoreQuad color [board ! (x - i, y - i) | i <- [0..3]] | y <- [4..rows], x <- [4..columns]]

scoreQuad :: Color -> [Color] -> Int
scoreQuad color colors = if (and [ c == Empty || c == color | c <- colors])
                         then if (length $ filter (== color) colors) == 3 then 1 else 0
                         else 0

-- Generate all possible next move positions
-- Uses possibleMoves and makeMove
-- moves :: Board -> Color -> [Board]
