toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x
  | x > 0     = toDigits (x `div` 10) ++ [x `mod` 10]
  | otherwise = []


toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []     = []
doubleEveryOther [x]    = [x]
doubleEveryOther (x:y:xs) = x:(y * 2):doubleEveryOther(xs)


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x > 10 = sumDigits ((toDigits x) ++ xs)
  | otherwise = x + (sumDigits xs)


validate :: Integer -> Bool
validate x =
  let
    digits    = toDigitsRev x
    doubled   = doubleEveryOther digits
    summed    = sumDigits doubled
    remainder = summed `mod` 10
  in
    remainder == 0
