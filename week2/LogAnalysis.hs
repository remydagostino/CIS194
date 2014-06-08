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
parseLines (x:xs) = (parseMessage x) : parseLines xs

parse :: String -> [LogMessage]
parse s = parseLines (lines s)

