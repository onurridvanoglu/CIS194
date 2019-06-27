-- Homework 1
import Data.Char
import Data.List (sum)
import Data.Digits
import Data.List.Ordered

toDigits :: Integer -> [Integer]
toDigits n
    | n == 0 = []
    | otherwise = map (fromIntegral . digitToInt) . show $ n 

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer]-> [Integer]
doubleEveryOther n = reverse $ skipFirst $ reverse n
    where skipFirst [] = []
          skipFirst [x] = [x]
          skipFirst (x : x1 : xs) = x : x1 * 2 : skipFirst xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

validate :: Integer -> Bool
validate n = if mod (sumDigits . doubleEveryOther . toDigits $ n) 10 == 0 then True else False

-- Towers of Hanoi
type Peg = String 
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 peg1 peg2 peg3 = [(peg1, peg2)]
hanoi d peg1 peg2 peg3 = hanoi (d - 1) peg1 peg3 peg2 ++ hanoi 1 peg1 peg2 peg3 ++ hanoi (d - 1) peg3 peg2 peg1 