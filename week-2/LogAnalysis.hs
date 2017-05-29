{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

testList = [LogMessage Info 5053 "pci_id: con ing!",LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)",LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled",LogMessage Info 4076 "verse.'",LogMessage Info 4764 "He trusts to you to set them free,",LogMessage Info 858 "your pocket?' he went on, turning to Alice.",LogMessage Info 898 "would be offended again.",LogMessage Info 3753 "pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all)",LogMessage Info 790 "those long words, and, what's more, I don't believe you do either!' And",LogMessage Info 3899 "hastily.",LogMessage Info 2194 "little creature, and held out its arms and legs in all directions, 'just",LogMessage Info 1447 "she was terribly frightened all the time at the thought that it might be",LogMessage Info 1147 "began ordering people about like that!'",LogMessage Info 3466 "pci_hcd beed VRAM=2)",LogMessage Info 3974 "#55500:00000 (nux Us nel chablesen ster C)",LogMessage Info 3724 "Laughing and Grief, they used to say.'",LogMessage Info 1283 "'Now tell me, Pat, what's that in the window?'",LogMessage Info 4469 "'If that's all you know about it, you may stand down,' continued the",LogMessage Info 1641 "'I feared it might injure the brain;",LogMessage Info 1744 "aloud; and in another moment it was out of sight."]

parseMessage :: String -> LogMessage
parseMessage ('I':s) = LogMessage Info (read (head $ words s) :: Int) (unwords $ tail $ words s)
parseMessage ('W':s) = LogMessage Warning (read (head $ words s) :: Int) (unwords $ tail $ words s)
parseMessage ('E':s) = LogMessage (Error (read (head $ words s) :: Int)) (read (head $ tail $ words s) :: Int) (unwords $ tail $ tail $ words s)
parseMessage x = Unknown x

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert mx@(LogMessage _ tx _) t@(Node l tm@(LogMessage _ ty _) r) | (tx > ty) = Node l tm (insert mx r)
                                                                  | (tx < ty) = Node (insert mx l) tm r
                                                                  | otherwise = t
insert m Leaf         = Node Leaf m Leaf
insert (Unknown _) mt = mt

build :: [LogMessage] -> MessageTree
build []      = Leaf
build (x:xss) = insert x (build xss)

printTree :: MessageTree -> String
printTree Leaf = "Blank"
printTree (Node l (LogMessage _ t _) r) = unwords [printTree l, show t, printTree r]

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

justTheTime :: [LogMessage] -> [Int]
justTheTime ((LogMessage _ t _):xss) = t:(justTheTime xss)
justTheTime _                        = []

justTheMessage :: [LogMessage] -> [String]
justTheMessage (LogMessage _ _ s : xss) = s : justTheMessage xss
justTheMessage _                        = []

severityGreaterThan :: Int -> LogMessage -> Bool
severityGreaterThan x (LogMessage (Error y) _ _) = (x < y)
severityGreaterThan _ _                          = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = justTheMessage $ inOrder $ build (filter (severityGreaterThan 50) xs)
