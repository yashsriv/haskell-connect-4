module AI where

import Control.Concurrent (threadDelay)
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import System.Random

class GamePosition a where
  moves :: a -> [a]
  static :: a -> Int

gatherMax :: [(Int, (Maybe Int, Int))] -> (Int, (Maybe Int, Int)) -> [(Int, (Maybe Int, Int))]
gatherMax [] x = x:[]
gatherMax xs@((_, (_, bscore)):_) x@(_, (_, score)) =
  if score == bscore
  then x:xs
  else if (score < bscore)
       then x:[]
       else xs

negmax :: (GamePosition a) => Int -> a -> IO (Maybe Int, Int)
negmax depth b
 | depth == 0 = return (Nothing, static b)
 | null (moves b) = return (Nothing, static b)
 | otherwise = do
     results <- sequence $ map (negmax (depth - 1)) (moves b)
     let zipped = zip [0..] results
         bests = foldl gatherMax [] zipped
     best <- getStdRandom (randomPick bests)
     let (moveIndex, (_, score)) = best
     return (Just moveIndex, negate score)

randomPick :: RandomGen g => [a] -> g -> (a, g)
randomPick xs gen = let (index, gen') = randomR (0, (length xs) - 1) gen
                    in (xs !! index, gen')

ioplayer :: (Eq a, Read a, Show a, GamePosition b) => [a] -> b -> IO a
ioplayer moves b =
  do putStr "Possible Moves: "
     putStrLn $ unwords $ map show moves
     putStrLn "Enter a valid move: "
     moveString <- getLine
     let move = read moveString
     if (move `elem` moves)
       then return move
       else do putStrLn $ "You have entered an invalid move: " ++ (show move)
               putStrLn "Please choose one of the possible moves."
               ioplayer moves b

randplayer :: [a] -> b -> IO a
randplayer moves _ = do threadDelay 500000
                        getStdRandom (randomPick moves)

negmaxplayer :: GamePosition b => Int -> [a] -> b -> IO a
negmaxplayer depth moves b =
  do
    (Just index, _) <- negmax depth b
    return $ moves !! index
