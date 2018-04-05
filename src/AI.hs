module AI where

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

maximise' :: Ord a => Tree a -> [a]
maximise' (Node n []) = [n]
maximise' (Node n ns)
             = mapmin (map minimise' ns)

minimise' :: Ord a => Tree a -> [a]
minimise' (Node n []) = [n]
minimise' (Node n ns)
             = mapmax (map maximise' ns)

mapmin :: Ord a => [[a]] -> [a]
mapmin (nums:rest) = n:(omit n rest)
   where n = minimum nums

mapmax :: Ord a => [[a]] -> [a]
mapmax (nums:rest) = n:(omit n rest)
  where n = maximum nums

omit :: Ord a => a -> [[a]] -> [a]
omit n [] = []
omit n (nums:rest) =
  if minleq nums n then omit n rest
                   else (minimum nums):(omit n rest)

minleq :: Ord a => [a] -> a -> Bool
minleq [] n = False
minleq (num:nums) n = if num <= n then True
                         else minleq nums n

evaluate :: GamePosition a => Int -> a -> Int
evaluate depth = maximum . maximise' . maptree static . prune depth . gametree
