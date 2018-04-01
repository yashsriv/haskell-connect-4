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

maxHeight = 6
maxCol = 7

-- Enumeration for the 3 Colors
data Color = Empty | Red | Yellow deriving (Eq)

-- Board is just a 2 Dimensional List with heights
data Board = Board [[Color]] [Int]

type Column = Int

initialBoard = Board (replicate maxCol (replicate maxHeight Empty)) (replicate maxCol 0)

-- Returns Columns whose topmost row is still not filled
possibleMoves :: Board -> [Column]
possibleMoves (Board _ heights)= map (fst) $ filter (\(_, h) -> h /= maxHeight) $ zip [1..] heights

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
