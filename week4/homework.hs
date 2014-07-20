-- Homework Exercises Wk 4

module Homework4 where

-- Exercise 1 - Wholemeal programming

-- | Fun1 (not yet refactored)
-- >>> fun1 []
-- 1
-- >>> fun1 [1,2,3,4]
-- 0
-- >>> fun1 [10,11,12]
-- 80
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs


-- | Fun1 (refactored)
-- >>> fun1' []
-- 1
-- >>> fun1' [1,2,3,4]
-- 0
-- >>> fun1' [10,11,12]
-- 80
fun1' :: [Integer] -> Integer
fun1' = foldl (\m x -> m * (x - 2)) 1
  . filter even


-- | Fun2 (not yet refactored)
-- >>> fun2 1
-- 0
-- >>> fun2 4
-- 6
-- >>> fun2 100
-- 688
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)


-- | fun2' (refactored)
-- >>> fun2' 1
-- 0
-- >>> fun2' 4
-- 6
-- >>> fun2' 100
-- 688
fun2' :: Integer -> Integer
fun2' = sum
  . filter even
  . takeWhile (> 1)
  . iterate (\a -> if even a then a `div` 2 else 3 * a + 1)


-- Exercise 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
     deriving (Show, Eq)

foldNode :: Tree a -> a -> Tree a
foldNode Leaf a                                          = Node 0 Leaf a Leaf
foldNode (Node h Leaf b Leaf) a                          = Node (h + 1) Leaf b (Node h Leaf b Leaf)
foldNode (Node h Leaf b right@(Node ih _ _ _)) a         = Node h (Node ih Leaf a Leaf) b right
foldNode (Node h left@(Node ih _ _ _) b Leaf) a          = Node h left b (Node ih Leaf a Leaf)
foldNode node@(Node h (Node _ _ _ _) b (Node _ _ _ _)) a = Node (h + 1) node a Leaf

-- If the tree is full we have to split it and put the odd one on the far right
-- split a tree by increasing the `height` of every node by 1 and inserting a 0 on the far right
-- Otherwise we need to fight the rightmost leaf and replace it with a node

foldTree :: [a] -> Tree a
foldTree = foldl foldNode Leaf

