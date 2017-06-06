-- Exercise 1

listLength :: [a] -> Integer
listLength []     = 0
listLength (_:xs) = 1 + listLength xs

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x < 1     = []
  | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

revIntList :: [Integer] -> [Integer]
revIntList []     = []
revIntList (x:xs) = (revIntList xs) ++ [x]

toDigits :: Integer -> [Integer]
toDigits x
  | x < 1     = []
  | otherwise = revIntList $ toDigitsRev x

-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther [x]      = [x]
doubleEveryOther (x:y:xs)
  | ((listLength (x:y:xs)) `mod` 2 == 0) = x * 2 : y : doubleEveryOther xs
  | otherwise                            = x : y * 2 : doubleEveryOther xs

-- Exercise 3

sumList :: [Integer] -> Integer
sumList []     = 0
sumList (x:xs) = x + sumList xs

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = (sumList $ toDigits x) + (sumDigits xs)

-- Exercise 4

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) x `mod` 10 == 0

-- Exercise 5

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n p1 p2 p3 = hanoi (n-1) p1 p3 p2 ++ [(p1,p2)] ++ hanoi (n-1) p3 p2 p1
