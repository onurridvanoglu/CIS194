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

