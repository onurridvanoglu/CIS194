module Golf where

-- Exercise 1
skips :: [a] -> [[a]]
skips xs = [func n xs | n <- [1 .. length xs]]

func :: Int -> [a] -> [a]
func n xs = [xs !! i | i <- [n - 1, n - 1 + n .. length xs - 1]]

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x : x1 : x2 : xs) 
    | x1 > x && x1 > x2 = x1 : localMaxima (x1 : x2 : xs)
    | otherwise = localMaxima (x1 : x2 : xs)
localMaxima _ = []
