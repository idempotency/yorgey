module Golf where

-- Exercise 1

skips :: [a] -> [[a]]
skips xs = map (skip xs) [1..length xs]

skip :: [a] -> Int -> [a]
skip xs n = case drop (n-1) xs
            of (y:ys) -> y : skip ys n
               []     -> []

-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima (x:xs@(y:z:xss))
  | (y > x && y > z) = y : (localMaxima xs)
  | otherwise        = localMaxima xs
localMaxima _ = []
