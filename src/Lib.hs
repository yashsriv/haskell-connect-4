module Lib
    ( begin,
    ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict((!))

import Board (
  Board (Board), Column,
  possibleMoves, makeMove, checkWin,
  initialBoard
    )

import AI

play :: Board -> ([Column] -> IO Column) -> ([Column] -> IO Column) -> Int -> IO ()
play b@(Board board h _) player1 player2 ply =
  do let moves = possibleMoves b
     putStrLn $ show b
     move <- if (ply == 0) then (player1 moves) else (player2 moves)
     let nBoard = makeMove b move
         hasWon = checkWin nBoard
     if hasWon then showVictory nBoard else play nBoard player1 player2 (1 - ply)

showVictory :: Board -> IO ()
showVictory b@(Board _ _ c) = do putStrLn $ show b
                                 putStrLn ((show c) ++ " has lost...")


choosePlayer :: (Eq a, Read a, Show a) => Int -> IO ([a] -> IO a)
choosePlayer i = do putStrLn $ "Choose Player " ++ (show i) ++ ":"
                    putStrLn "1. Human"
                    putStrLn "2. Noob Computer"
                    putStrLn "Enter Choice: "
                    choiceString <- getLine
                    let choice = read choiceString
                    case choice of
                      1 -> return ioplayer
                      2 -> return randplayer
                      _ -> do putStrLn $ "You have entered an invalid choice: " ++ (show choice)
                              putStrLn "Please choose one of the possible choices."
                              choosePlayer i

begin :: IO ()
begin = do
  player1 <- choosePlayer 1
  player2 <- choosePlayer 2
  play initialBoard player1 player2 0
