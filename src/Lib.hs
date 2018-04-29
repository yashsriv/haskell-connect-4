module Lib where
    -- ( begin,
    -- ) where

-- import qualified Data.Map.Strict as Map
-- import Data.Map.Strict((!))

-- import Board (
--   Board (Board), Column,
--   possibleMoves, makeMove, checkWin,
--   initialBoard
--     )

-- import AI

-- play :: Board -> ([Column] -> Board -> IO Column) -> ([Column] -> Board -> IO Column) -> Int -> IO ()
-- play b@(Board board _ _) player1 player2 ply =
--   do let moves = possibleMoves b
--      putStrLn $ show b
--      if null moves
--        then putStrLn "It's a draw!!"
--        else do
--        move <- if (ply == 0) then (player1 moves b) else (player2 moves b)
--        let nBoard = makeMove b move
--            hasWon = checkWin nBoard
--        if hasWon
--          then showVictory nBoard
--          else play nBoard player1 player2 (1 - ply)

-- showVictory :: Board -> IO ()
-- showVictory b@(Board _ _ c) = do putStrLn $ show b
--                                  putStrLn ((show c) ++ " has lost...")


-- choosePlayer :: (Eq a, Read a, Show a, GamePosition b) => Int -> IO ([a] -> b -> IO a)
-- choosePlayer i = do putStrLn $ "Choose Player " ++ (show i) ++ ":"
--                     putStrLn "1. Human"
--                     putStrLn "2. Noob Computer"
--                     putStrLn "3. Easy Computer"
--                     putStrLn "4. Normal Computer"
--                     putStrLn "5. Hard Computer"
--                     putStrLn "Enter Choice: "
--                     choiceString <- getLine
--                     let choice = read choiceString
--                     case choice of
--                       1 -> return ioplayer
--                       2 -> return randplayer
--                       3 -> return (negmaxplayer 3)
--                       4 -> return (negmaxplayer 5)
--                       5 -> return (negmaxplayer 7)
--                       _ -> do putStrLn $ "You have entered an invalid choice: " ++ (show choice)
--                               putStrLn "Please choose one of the possible choices."
--                               choosePlayer i

-- begin :: IO ()
-- begin = do
--   player1 <- choosePlayer 1
--   player2 <- choosePlayer 2
--   play initialBoard player1 player2 0
