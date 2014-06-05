module Hanoi where

type Peg = String
type Move = (Peg, Peg)

-- | Compute Towers of Hanoi
--
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"), ("a","b"), ("c","b")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi count p1 p2 p3 =
  let
    state = ([1..count], [], [])
  in
    -- Dummy
    [(p1, p2)]


-- define some function where i can store the state 
-- store state as [Int] [Int] [Int]
-- [1,2] [] []
-- [2] [1] []
-- [] [1] [2]
-- [] [] [1,2]
