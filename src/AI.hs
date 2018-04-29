module AI where

import Control.Concurrent (threadDelay)
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import System.Random

class GamePosition a where
  moves :: a -> [a]
  static :: a -> Int

-- We take maximum of the negative of the scores
gatherMax :: [(Int, ((Maybe Int, Int), a))] -> (Int, ((Maybe Int, Int), a)) -> [(Int, ((Maybe Int, Int), a))]
gatherMax [] x = x:[]
gatherMax xs@((_, ((_, bscore), _)):_) x@(_, ((_, score), _)) =
  if score == bscore
  then x:xs
  else if (score < bscore)
       then x:[]
       else xs

genRandom :: (RandomGen g) => g -> Int -> [StdGen] -> ([StdGen], g)
genRandom orig 0 gs = (gs, orig)
genRandom orig x gs = let (seed, orig') = random orig
                          g = mkStdGen seed
                      in genRandom orig' (x - 1) (g:gs)

negmax :: (GamePosition a, RandomGen g) => Int -> a -> g -> ((Maybe Int, Int), g)
negmax depth b gen
 | depth == 0     = ((Nothing, static b), gen)
 | null (moves b) = ((Nothing, static b), gen)
 | otherwise =
     -- All possible moves
     let posMoves = moves b
         -- Generate a list of random generators -> one to pass down to each move
         (gens, gen') = genRandom gen (length posMoves) []
         -- zipWith is used to pass corresponding generator to corresponding move
         results = zipWith (\g -> \mov -> negmax (depth - 1) mov g) gens (moves b)
         -- Associate move index with the results (i.e. playing which move will result in this)
         zipped = zip [0..] results
         -- Obtain subset of best scores
         bests = foldl gatherMax [] zipped
         -- Randomly pick any move of the best moves
         (best, gen'') = randomPick bests gen'
         -- Extract the move index and the score associated with the move
         (moveIndex, ((_, score), _)) = best
         -- Return move index, negative of the score and the modified generator
     in ((Just moveIndex, negate score), gen'')

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
    (Just index, _) <- getStdRandom (negmax depth b)
    return $ moves !! index
