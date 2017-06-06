{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage ('I':s) = LogMessage Info (read (head $ words s) :: Int) (unwords $ tail $ words s)
parseMessage ('W':s) = LogMessage Warning (read (head $ words s) :: Int) (unwords $ tail $ words s)
parseMessage ('E':s) = LogMessage (Error (read (head $ words s) :: Int)) (read (head $ tail $ words s) :: Int) (unwords $ tail $ tail $ words s)
parseMessage x = Unknown x

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert m Leaf         = Node Leaf m Leaf
insert mx@(LogMessage _ tx _) t@(Node l tm@(LogMessage _ ty _) r)
  | (tx > ty) = Node l tm (insert mx r)
  | (tx < ty) = Node (insert mx l) tm r
  | otherwise = t

-- Exercise 3

build :: [LogMessage] -> MessageTree
build []      = Leaf
build (x:xss) = insert x (build xss)

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

-- Exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = justTheMessage $ inOrder $ build (filter (severityGreaterThan 50) xs)

justTheMessage :: [LogMessage] -> [String]
justTheMessage (LogMessage _ _ s : xss) = s : justTheMessage xss
justTheMessage _                        = []

severityGreaterThan :: Int -> LogMessage -> Bool
severityGreaterThan x (LogMessage (Error y) _ _) = (x < y)
severityGreaterThan _ _                          = False

-- Bonus

printTree :: MessageTree -> String
printTree Leaf = "Blank"
printTree (Node l (LogMessage _ t _) r) = unwords [printTree l, show t, printTree r]

justTheTime :: [LogMessage] -> [Int]
justTheTime ((LogMessage _ t _):xss) = t:(justTheTime xss)
justTheTime _                        = []
