module Lecture where

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

mySum :: [Integer] -> Integer
mySum []     = 0
mySum (x:xs) = x + mySum xs

myProduct :: [Integer] -> Integer
myProduct []     = 1
myProduct (x:xs) = x * myProduct xs

myLength :: [a] -> Integer
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

myFold :: b -> (a -> b -> b) -> [a] -> b
myFold z f []     = z
myFold z f (x:xs) = f x (myFold z f xs)

-- Fold examples have been expanded for better understanding.
fSum      = myFold 0  (\x acc -> x + acc)
fProduct  = myFold 1  (\x acc -> x * acc)
fLength   = myFold 0  (\_ acc -> 1 + acc)
fMap f    = myFold [] (\x acc -> (f x):acc)
