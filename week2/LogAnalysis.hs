{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage ""   = Unknown ""
parseMessage (s) =
  case (words s) of
    ("E":level:time:message) -> LogMessage (Error (read level :: Int)) (read time :: TimeStamp) (unwords message)
    ("I":time:message)       -> LogMessage Info (read time :: TimeStamp) (unwords message)
    ("W":time:message)       -> LogMessage Warning (read time :: TimeStamp) (unwords message)
    (message)                -> Unknown (unwords message)

parseLines :: [String] -> [LogMessage]
parseLines []     = []
parseLines (x:xs) = (parseMessage x) : parseLines xs

parse :: String -> [LogMessage]
parse s = parseLines (lines s)


-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert logm Leaf = Node Leaf logm Leaf
insert logm@(LogMessage _ time _) (Node left cur@(LogMessage _ currentTime _) right)
  | time > currentTime = Node left cur (insert logm right)
  | otherwise          = Node (insert logm left) cur right
insert _ tree = tree


-- Exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)


-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logm right) = (inOrder left) ++ [logm] ++ (inOrder right)


-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs =
  let
    ordered = inOrder (build xs)
  in
    [message | (LogMessage (Error level) _ message) <- ordered, level > 50]
