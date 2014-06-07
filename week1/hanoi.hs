module Hanoi where

type Peg = String
type Move = (Peg, Peg)

-- Example 3 piece game
-- 0. [1,2,3] [], []
-- 1. [2,3] [1] []
-- 2. [3] [1] [2]
-- 3. [3] [] [1,2]
-- 4. [] [3] [1,2]
-- 5. [1] [3] [2]
-- 6. [1] [2,3] []
-- 7. [] [1,2,3] []

-- Algorithm for 2 piece game
-- hanoi 2 a b c
-- (hanoi 1 a c b) + (a, b) + (hanoi 1 c b a)
-- ((hanoi 0) + (a, c)) + (a, b) + ((hanoi 0) + (c, b) + (hanoi 0))
-- (a, c) + (a, b) + (c, b)

-- | Compute Towers of Hanoi
--
-- >>> hanoi 1 "a" "b" "c"
-- [("a","b")]
--
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"),("a","b"),("c","b")]
--
-- >>> hanoi 3 "a" "b" "c"
-- [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _  = []

hanoi x start finish temp =
  let
    topToTemp = hanoi (x - 1) start temp finish
    myMove = (start, finish)
    tempToFinish = hanoi (x - 1) temp finish start
  in
    topToTemp ++ [myMove] ++ tempmpToFinish




