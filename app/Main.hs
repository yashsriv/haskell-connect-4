module Main where

-- import Board (initialBoard, Board.Board, Board)
import qualified Board
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Map.Strict((!))
import AI
import System.Exit

transRed :: Color
transRed = makeColorI 255 0 0 192

transYellow :: Color
transYellow = makeColorI 255 255 0 192

moveFunc :: ([Board.Column] -> Board.Board -> Board.Column -> IO Board.Column) -> ([Board.Column] -> Board.Board -> Board.Column -> IO Board.Column) -> Event -> Board.Board -> IO Board.Board
moveFunc player1 player2 (EventKey (MouseButton LeftButton) Up _ (coordX, _)) b@(Board.Board board h c Board.Empty) = let col = ceiling $ (coordX + 350) / 100
                                                                                                                          moves = Board.possibleMoves b
                                                                                                                      in if null moves
                                                                                                                         then return (Board.Board board h c Board.Both) 
                                                                                                                         else moveFunc' moves c player1 player2 b col
moveFunc player1 player2 (EventKey (MouseButton LeftButton) Up _ _) _ = exitSuccess
moveFunc player1 player2 (EventKey (SpecialKey KeyEsc) Up _ _) _ = die "Game Aborted"
moveFunc player1 player2 (EventKey (Char 'q') Up _ _) _ = die "Game Aborted"
moveFunc player1 player2 _ b = return b

moveFunc' moves c player1 player2 b col =
    do
        move <- if (c == Board.Red) then (player1 moves b col) else (player2 moves b col)
        let nBoard@(Board.Board board h c _) = Board.makeMove b move
            hasWon = Board.checkWin nBoard
        if hasWon
        then return (Board.Board board h c (Board.opp c))
        else return nBoard

timeFunc :: Float -> Board.Board -> IO Board.Board
timeFunc _ b = return b

drawBoard' :: Board.Board -> Picture
drawBoard' (Board.Board board _ _ _) = pictures [translate ((fromIntegral (x - 4)) * 100.0) ((fromIntegral (2 * y - 7)) * 50.0) $ color (convColor (board ! (x, y))) $ circleSolid 40 | x <- [1..Board.columns], y <- [1..Board.rows]]

drawBoard :: Board.Board -> IO Picture
drawBoard b@(Board.Board _ _ _ Board.Empty)  = return $ drawBoard' b
drawBoard b@(Board.Board _ _ _ Board.Red)    = return $ pictures [color transRed $ rectangleSolid 700 600, drawBoard' b]
drawBoard b@(Board.Board _ _ _ Board.Yellow) = return $ pictures [color transYellow $ rectangleSolid 700 600, drawBoard' b]
drawBoard b@(Board.Board _ _ _ Board.Both)   = return $ pictures [color transYellow $ rectangleSolid 700 600, color transRed $ rectangleUpperSolid 700 600, drawBoard' b]

convColor :: Board.Color -> Color
convColor Board.Empty = white
convColor Board.Red = red
convColor Board.Yellow = yellow

window :: Display
window = InWindow "Connect 4 !!" (700, 600) (10, 10)

background :: Color
background = dark blue

drawing :: IO Picture
drawing = drawBoard $ Board.makeMove Board.initialBoard 3

choosePlayer :: (Eq a, Read a, Show a, GamePosition b) => Int -> IO ([a] -> b -> a-> IO a)
choosePlayer i = do putStrLn $ "Choose Player " ++ (show i) ++ ":"
                    putStrLn "1. Human"
                    putStrLn "2. Noob Computer"
                    putStrLn "3. Easy Computer"
                    putStrLn "4. Normal Computer"
                    putStrLn "5. Hard Computer"
                    putStrLn "Enter Choice: "
                    choiceString <- getLine
                    let choice = read choiceString
                    case choice of
                      1 -> return ioplayer
                      2 -> return randplayer
                      3 -> return (negmaxplayer 1)
                      4 -> return (negmaxplayer 3)
                      5 -> return (negmaxplayer 5)
                      _ -> do putStrLn $ "You have entered an invalid choice: " ++ (show choice)
                              putStrLn "Please choose one of the possible choices."
                              choosePlayer i

begin :: IO ()
begin = do
  player1 <- choosePlayer 1
  player2 <- choosePlayer 2
  playIO window background 1 Board.initialBoard drawBoard (moveFunc player1 player2) timeFunc

main :: IO ()
main = begin
-- main = playIO window background 1 Board.initialBoard drawBoard (moveFunc randplayer randplayer) timeFunc