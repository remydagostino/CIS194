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

treeHeight :: Tree a -> Integer
treeHeight Leaf           = 0
treeHeight (Node h _ _ _) = h

-- | foldNode
-- >>> foldNode 'a' Leaf
-- Node 0 Leaf 'a' Leaf
--
-- >>> foldNode 'a' (Node 0 Leaf 'b' Leaf)
-- Node 1 (Node 0 Leaf 'a' Leaf) 'b' Leaf
--
-- >>> foldNode 'a' (Node 1 Leaf 'b' (Node 0 Leaf 'c' Leaf))
-- Node 1 (Node 0 Leaf 'a' Leaf) 'b' (Node 0 Leaf 'c' Leaf)
--
-- >>> foldNode 'a' (Node 1 (Node 0 Leaf 'c' Leaf) 'b' Leaf)
-- Node 1 (Node 0 Leaf 'c' Leaf) 'b' (Node 0 Leaf 'a' Leaf)
--
-- >>> foldNode 'a' (Node 1 (Node 0 Leaf 'c' Leaf) 'b' (Node 0 Leaf 'd' Leaf))
-- Node 2 (Node 1 (Node 0 Leaf 'a' Leaf) 'c' Leaf) 'b' (Node 0 Leaf 'd' Leaf)
--
foldNode :: a -> Tree a -> Tree a
-- Leaf
foldNode a Leaf =
  Node 0 Leaf a Leaf
-- Leafy Node
foldNode a (Node h Leaf b Leaf) =
  Node (h + 1) (foldNode a Leaf) b Leaf
-- Leaf on the left
foldNode a (Node h Leaf b right@(Node _ _ _ _)) =
  Node h (foldNode a Leaf) b right
-- Leaf on the right
foldNode a (Node h left@(Node _ _ _ _) b Leaf) =
  Node h left b (foldNode a Leaf)
-- Nodes everywhere
foldNode a (Node h left@(Node hl _ _ _) b right@(Node hr _ _ _)) =
  let (newL, newR) = if hr < hl
                     then (left, (foldNode a right))
                     else ((foldNode a left), right)
  in
    Node ((max (treeHeight newL) (treeHeight newR)) + 1) newL b newR

foldTree :: [a] -> Tree a
foldTree = foldr foldNode Leaf


-- | Exercise 3 : xor
--
-- >>> xor [False, True, False]
-- True
--
-- >>> xor [False, True, False, False, True]
-- False
--
xor :: [Bool] -> Bool
xor = foldr (\v m -> if v == True then not m else m) False

-- | Exercise 3 : map
map' :: (a -> b) -> [a] -> [b]
map' f a = foldr (\v m -> (f v):m) [] a

