module Board (
  maxHeight,
  maxCol,
  Color (Red, Yellow),
  Board (Board),
  Column,
  initialBoard,
  possibleMoves,
  makeMove,
  ) where

import qualified Data.Map.Strict as Map

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
possibleMoves (Board _ heights)= [x | x <- [1..columns], (heights ! (x, rows)) == Empty]

-- Update the Board
makeMove :: Board -> Color -> Column -> Board
makeMove (Board board heights) color col = let left = take (col - 1) board
                                               right = drop col board
                                               curHeight = heights !! (col - 1)
                                               curCol = board !! (col - 1)
                                               current = take curHeight curCol ++ color : (replicate (maxHeight - curHeight - 1) Empty)
                                               nboard = left ++ current:right
                                               nheights = (take (col - 1) heights) ++ (curHeight + 1):(drop col heights)
                                           in Board nboard nheights

-- Check if a player of particular color has won
-- checkWin :: Board -> Color -> Bool

-- Calculate value of a particular board w.r.t to Color
-- valuation :: Board -> Color -> Int

-- Generate all possible next move positions
-- Uses possibleMoves and makeMove
-- moves :: Board -> Color -> [Board]
