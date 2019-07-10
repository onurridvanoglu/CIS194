{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage 
parseMessage string = case words string of
    ("I" : ts : message) -> LogMessage Info (read ts) (unwords message)
    ("W" : ts : message) -> LogMessage Warning (read ts) (unwords message)
    ("E" : lvl : ts : message) -> LogMessage (Error $ read lvl) (read ts) (unwords message)
    _ -> Unknown (unwords $ words string)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insertMsg :: LogMessage -> MessageTree -> MessageTree
insertMsg lmsg Leaf = Node Leaf lmsg Leaf
insertMsg (Unknown _) tree = tree
insertMsg lmsg1 @ (LogMessage _ ts1 _) (Node l lmsg2 @ (LogMessage _ ts2 _) r)
    | ts1 > ts2 = Node l lmsg2 (insertMsg lmsg1 r)
    | otherwise = Node (insertMsg lmsg1 l) lmsg2 r
insertMsg _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insertMsg Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l x r) = inOrder l ++ [x] ++ inOrder r