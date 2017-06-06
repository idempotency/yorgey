module Golf where

data IntList = Empty
             | Cons Int IntList
  deriving Show

data List t = E
            | C t (List t)
  deriving Show

doubleAll :: IntList -> IntList
doubleAll Empty        = Empty
doubleAll (Cons x xss) = Cons (x*2) $ doubleAll xss

intListMap :: IntList -> (Int -> Int) -> IntList
intListMap Empty _        = Empty
intListMap (Cons x xss) f = Cons (f x) (intListMap xss f)

intListFilter :: IntList -> (Int -> Bool) -> IntList
intListFilter Empty _        = Empty
intListFilter (Cons x xss) f | f x       = Cons x (intListFilter xss f)
                             | otherwise = intListFilter xss f

listMap :: List a -> (a -> b) -> List b
listMap E _       = E
listMap (C x xss) f = C (f x) (listMap xss f)

listFilter :: List a -> (a -> Bool) -> List a
listFilter E _      = E
listFilter (C x xss) f | f x       = C x (listFilter xss f)
                       | otherwise = listFilter xss f

skips :: [a] -> [[a]]
skips xs = map (skip xs) [1..length xs]

skip :: [a] -> Int -> [a]
skip xs n = case drop (n-1) xs of
              (y:ys) -> y : skip ys n
              []     -> []

localMaxima :: [Integer] -> [Integer]
localMaxima (x:xs@(y:z:xss)) | (y > x && y > z) = y : (localMaxima xs)
                             | otherwise        = localMaxima xs
localMaxima _ = []
