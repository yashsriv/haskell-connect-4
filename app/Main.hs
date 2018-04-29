module Main where

import System.Exit

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import AI (GamePosition)
import Board hiding (Color)
import GraphicsBoard (drawBoard)
import Player

moveFunc :: (Player Column Board) -> (Player Column Board) -> Event -> Board -> IO Board
moveFunc player1 player2 (EventKey (MouseButton LeftButton) Up _ (coordX, _)) b@(Board board h c Empty) = let col = ceiling $ (coordX + 350) / 100
                                                                                                              moves = possibleMoves b
                                                                                                          in if null moves
                                                                                                             then return (Board board h c Both)  -- draw
                                                                                                             else moveFunc' moves c player1 player2 b col
moveFunc player1 player2 (EventKey (MouseButton LeftButton) Up _ _) _ = exitSuccess
moveFunc player1 player2 (EventKey (SpecialKey KeyEsc) Up _ _) _      = die "Game Aborted"
moveFunc player1 player2 (EventKey (Char 'q') Up _ _) _               = die "Game Aborted"
moveFunc player1 player2 _ b                                          = return b

moveFunc' moves c player1 player2 b col =
    do
        move <- if (c == Red) then (player1 moves b (Just col)) else (player2 moves b (Just col))
        let nBoard@(Board board h c _) = makeMove b move
            hasWon = checkWin nBoard
        if hasWon
        then return (Board board h c (opp c))
        else return nBoard

timeFunc :: Float -> Board -> IO Board
timeFunc _ b = return b


window :: Display
window = InWindow "Connect 4 !!" (700, 600) (10, 10)

background :: Color
background = dark blue

choosePlayer :: GamePosition b => Int -> IO (Player a b)
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
  playIO window background 1 initialBoard drawBoard (moveFunc player1 player2) timeFunc

main :: IO ()
main = begin
