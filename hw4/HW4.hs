{-# LANGUAGE BangPatterns #-}
module HW4 where

-- import GHC.Exts
import Data.List

-- Exercise 1
fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate f
            where f n | even n    = n `div` 2
                      | otherwise = 3 * n + 1

-- Exercise 2
data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving Eq

instance Show a => (Show (Tree a)) where
    show Leaf                 = ""
    show (Node 0 Leaf y Leaf) = show y ++ "[0]\n"
    show (Node n s y t)       = nodeName ++
       (unlines . fstBranch t . lines . show) s ++
       (unlines . sndBranch . lines . show) t
         where nodeName    = show y ++ "[" ++ show n ++ "]\n"
               indent      = replicate ((length nodeName `div` 2) - 1) ' '
               fstBranch r = case r of
                                Leaf -> zipWith (++) (map (indent ++)
                                            ("└":repeat " "))
                                _    -> zipWith (++) (map (indent ++)
                                            ("├":repeat "│"))
               sndBranch   = zipWith (++) (map (indent ++) ("└":repeat " "))

getDepth :: Tree t -> Integer
getDepth Leaf = -1
getDepth (Node n _ _ _) = n
foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf

treeInsert :: a -> Tree a -> Tree a
treeInsert x Leaf              = Node 0 Leaf x Leaf
treeInsert x (Node 0 _ y _)    = Node 1 (Node 0 Leaf x Leaf) y Leaf
treeInsert x (Node 1 s y Leaf) = Node 1 s y (Node 0 Leaf x Leaf)
treeInsert x (Node n s@(Node k _ _ _) y t@(Node l _ _ _))
      | k < l = Node n (treeInsert x s) y t
      | k > l = Node n s y (treeInsert x t)
      | otherwise = Node n' s y t'
            where n' = getDepth t' + 1
                  t' = treeInsert x t
treeInsert _ _ = error "Something's wrong with the tree."

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldl (\x y -> if y then not x else x) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ys -> f x : ys) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse

-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = remaining n $ removals n

removals :: Integer -> [Integer]
removals n = sort $
                [i + j + 2 * i * j | i <- [1..b], j <- [i..n `div` (2 * i)]]
                    where b = floor (sqrt . fromIntegral $ n :: Double)

remaining :: Integer -> [Integer] -> [Integer]
remaining n = go 1 []
                where go m xs []     = [2 * n + 1, 2 * n..2 * m + 1] ++ xs
                      go m xs (y:ys) | m == y = go (m + 1) xs (y:ys)
                                     | m > n  = xs
                                     | m > y  = go m xs ys
                                     | m < y  = go (m + 1)
                                                       (2 * m + 1 : xs) (y:ys)
