module GraphicsBoard where

import Data.Map.Strict((!))

import Graphics.Gloss

import qualified Board

transRed :: Color
transRed = makeColorI 255 0 0 192

transYellow :: Color
transYellow = makeColorI 255 255 0 192

convColor :: Board.Color -> Color
convColor Board.Empty = white
convColor Board.Red = red
convColor Board.Yellow = yellow

drawBoard' :: Board.Board -> Picture
drawBoard' (Board.Board board _ _ _) = pictures [translate ((fromIntegral (x - 4)) * 100.0) ((fromIntegral (2 * y - 7)) * 50.0) $ color (convColor (board ! (x, y))) $ circleSolid 40 | x <- [1..Board.columns], y <- [1..Board.rows]]

drawBoard :: Board.Board -> IO Picture
drawBoard b@(Board.Board _ _ _ Board.Empty)  = return $ drawBoard' b
drawBoard b@(Board.Board _ _ _ Board.Red)    = return $ pictures [color transRed $ rectangleSolid 700 600, drawBoard' b]
drawBoard b@(Board.Board _ _ _ Board.Yellow) = return $ pictures [color transYellow $ rectangleSolid 700 600, drawBoard' b]
drawBoard b@(Board.Board _ _ _ Board.Both)   = return $ pictures [color transYellow $ rectangleSolid 700 600, color transRed $ rectangleUpperSolid 700 600, drawBoard' b]
