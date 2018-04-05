module Board (
  rows,
  columns,
  Color (Red, Yellow),
  Board (Board),
  Column,
  initialBoard,
  possibleMoves,
  makeMove,
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict((!))

rows = 6
columns = 7

type Column = Int
type Row = Int
type Coords = (Column, Row)

-- Enumeration for the 3 Colors
data Color = Empty | Red | Yellow deriving (Eq)

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
-- checkWin :: Board -> Color -> Bool

-- Calculate value of a particular board w.r.t to Color
-- valuation :: Board -> Color -> Int

-- Generate all possible next move positions
-- Uses possibleMoves and makeMove
-- moves :: Board -> Color -> [Board]
