module AI where

import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)

class GamePosition a where
  moves :: a -> [a]
  static :: a -> Int

data Tree a = Node a [Tree a]

reptree :: (a -> [a]) -> a -> Tree a
reptree f x = Node x (map (reptree f) (f x))

gametree :: GamePosition a => a -> Tree a
gametree p = reptree moves p

maptree :: (a -> Int) -> Tree a -> Tree Int
maptree f (Node x xs) = Node (f x) (map (maptree f) xs)

maximise :: Tree Int -> Int
maximise (Node n [])  = n
maximise (Node n sub) = maximum (map minimise sub)

minimise :: Tree Int -> Int
minimise (Node n [])  = n
minimise (Node n sub) = minimum (map maximise sub)

prune :: Int -> Tree a -> Tree a
prune 0 (Node n sub) = Node n []
prune k (Node n sub) = Node n (map (prune (k-1)) sub)

evaluate :: GamePosition a => Int -> a -> Int
evaluate depth = maximise . maptree static . prune depth . gametree


negmax :: GamePosition a => Int -> a -> (Maybe Int, Int)
negmax depth b
 | depth == 0 = (Nothing, static b)
 | null (moves b) = (Nothing, static b)
 | otherwise = (\(a, (b, c)) -> (Just a, c)) $ maximumBy (comparing (snd . snd)) $ zip [1..] $ map (\(a, b) -> (a, negate b)) $ map (negmax (depth - 1)) (moves b)
