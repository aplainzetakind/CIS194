{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage m = case words m of
          ("I":t:rest)   -> LogMessage Info (read t) (unwords rest)
          ("W":t:rest)   -> LogMessage Warning (read t) (unwords rest)
          ("E":s:t:rest) -> LogMessage (Error (read s)) (read t) (unwords rest)
          x              -> Unknown (unwords x)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) ms          = ms
insert m@(LogMessage _ t _) ms = case ms of
                      Leaf                          -> Node Leaf m Leaf
                      Node m1 n@(LogMessage _ t' _) m2
                                        | t' > t    -> Node (insert m m1) n m2
                                        | otherwise -> Node m1 n (insert m m2)
                      Node _ (Unknown _) _          -> error "Bad tree."

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf           = []
inOrder (Node m1 m m2) = inOrder m1 ++ [m] ++ inOrder m2

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extract . filter check . inOrder . build
                  where check (LogMessage (Error n) _ _) = n >= 50
                        check _                          = False
                        extract (LogMessage _ _ s)       = s
                        extract _                        = error "Error."
