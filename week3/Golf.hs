module Golf where

import Data.List
import Data.Maybe

-- Safe Head
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- Gets every nth item from a list
everyNth :: Int -> [a] -> [a]
everyNth _ [] = []
everyNth n xs
  | isJust first = fromJust first : (everyNth n (tail remaining))
  | otherwise    = []
  where remaining = drop (n - 1) xs
        first     = safeHead remaining

-- | Compute skips
--
-- >>> skips "ABCD"
-- ["ABCD","BD","C","D"]
--
-- >>> skips "hello!"
-- ["hello!","el!","l!","l","o","!"]
--
-- >>> skips [1]
-- [[1]]
--
-- >>> skips [True, False]
--[[True,False],[False]]
--
-- >>> skips []
-- []
skips :: [a] -> [[a]]
skips xs = (map (\n -> everyNth n xs)) [1..(length xs)]


-- | Compute local maxima
--
-- >>> localMaxima [2,9,5,6,1]
-- [9,6]
-- >>> localMaxima [2,3,4,1,5]
-- [4]
-- >>> localMaxima [1,2,3,4,5]
-- []
localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs)
  | isMaxima    = b : localMaxima rest
  | otherwise = localMaxima rest
  where
    isMaxima = length (filter (>b) [a,c]) == 0
    rest   = b:c:xs
localMaxima _ = []

-- Histogram Implementation
histogram :: [Integer] -> String
histogram (xs) = let
    base = (replicate 10 '=') ++ "\n" ++ ['0'..'9'] ++ "\n"
    counts =  (map $ (subtract 1) . length) . group . sort $ (xs ++ [0..9])
    maxCount = maximum counts
    hBars = map (\l -> (replicate l 'x') ++ (replicate (maxCount - l) ' ')) counts
    vBars = concat . intersperse "\n" . reverse . transpose $ hBars
  in
    vBars ++ "\n" ++ base







