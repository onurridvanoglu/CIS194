-- Finding the range of the Int values
biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

reallyBig :: Integer
reallyBig = 2 ^ (2 ^ (2 ^ (2 ^ 2)))

-- Determining the digit number of "reallyBig"
numDigits :: Int 
numDigits = length (show reallyBig)

-- Sum of number from 1 to n
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n - 1)

foo :: Integer -> Integer
foo 0 = 16
foo 1 
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0            = 0
  | n `mod` 17 == 2  = (- 43)
  | otherwise        = n + 3