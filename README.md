# haskell-connect-4
Implementation of Connect 4 in Haskell

## Application Spec

A board can be represented as such:
``` haskell
-- TypeDef for a columnn for more verbose code
type Column = Int

-- Enumeration for the 3 Colors
data Color = Empty | Red | Yellow

-- Board is just a 2 Dimensional List
data Board = Board [[Color]]
```

Functions Needed:
``` haskell
-- Returns Columns whose topmost row is still not filled
possibleMoves :: Board -> [Column]

-- Update the Board
makeMove :: Board -> Color -> Column -> Board

-- Check if a player of particular color has won
checkWin :: Board -> Color -> Bool

-- Calculate value of a particular board w.r.t to Color
valuation :: Board -> Color -> Int

-- Generate all possible next move positions
-- Uses possibleMoves and makeMove
moves :: Board -> Color -> [Board]
```
