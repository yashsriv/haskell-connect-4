module AI where

import Control.Concurrent (threadDelay)
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import System.Random

class GamePosition a where
  moves :: a -> [a]
  static :: a -> Int

negmax :: GamePosition a => Int -> a -> (Maybe Int, Int)
negmax depth b
 | depth == 0 = (Nothing, static b)
 | null (moves b) = (Nothing, static b)
 | otherwise = let results = map (negmax (depth - 1)) (moves b)
                   zipped = zip [1..] results
                   best = maximumBy (comparing (negate . snd . snd)) zipped
                   (moveNum, (_, score)) = best
                in (Just moveNum, negate score)

randomPick :: RandomGen g => [a] -> g -> (a, g)
randomPick xs gen = let (index, gen') = randomR (0, (length xs) - 1) gen
                    in (xs !! index, gen')

ioplayer :: (Eq a, Read a, Show a) => [a] -> IO a
ioplayer moves =
  do putStr "Possible Moves: "
     putStrLn $ unwords $ map show moves
     putStrLn "Enter a valid move: "
     moveString <- getLine
     let move = read moveString
     if (move `elem` moves)
       then return move
       else do putStrLn $ "You have entered an invalid move: " ++ (show move)
               putStrLn "Please choose one of the possible moves."
               ioplayer moves

randplayer :: [a] -> IO a
randplayer moves = do threadDelay 500000
                      getStdRandom (randomPick moves)
