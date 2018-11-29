{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module JoinList where

import Sized
import Data.Monoid
import Scrabble
import Data.Char
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a) deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) k l = Append (tag k <> tag l) k l

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single m x)   = Just x
indexJ _ (Single m x)   = Just x
indexJ n (Append m k l) | Size n >= size m       = Nothing
                        | Size n >= size (tag k) = indexJ (n - k') l
                        | otherwise              = indexJ n k
                            where k' = getSize . size . tag $ k

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n l     | n <= 0 = l
dropJ n (Single m x)   = Empty
dropJ n (Append m k l) | Size n >= size m       = Empty
                       | Size n >= size (tag k) = dropJ (n - k') l
                       | otherwise              = dropJ n k +++ l
                            where k' = getSize . size . tag $ k

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty          = Empty
takeJ n j     | n <= 0 = Empty
takeJ n j@(Single _ _) = j
takeJ n j@(Append m k l) | Size n >= size m       = j
                         | Size n >= size (tag k) = k +++ takeJ (n - k') l
                         | otherwise              = takeJ n k
                            where k' = getSize . size . tag $ k

score :: Char -> Score
score 'A' = 1
score 'B' = 3
score 'C' = 3
score 'D' = 2
score 'E' = 1
score 'F' = 4
score 'G' = 2
score 'H' = 4
score 'I' = 1
score 'J' = 8
score 'K' = 5
score 'L' = 1
score 'M' = 3
score 'N' = 1
score 'O' = 1
score 'P' = 3
score 'Q' = 10
score 'R' = 1
score 'S' = 1
score 'T' = 1
score 'U' = 1
score 'V' = 4
score 'W' = 4
score 'X' = 8
score 'Y' = 4
score 'Z' = 10
score _   = 0

scoreString :: String -> Score
scoreString = sum . map (score . toUpper)

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance  Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ k l) = toString k ++ toString l
  fromString = foldl (+++) Empty . map (\s -> Single (scoreString s, Size 1) s) . lines
  line = indexJ
  replaceLine n s l = takeJ (n - 1) l +++ fromString s +++ dropJ n l
  numLines = getSize . snd . tag
  value = fromIntegral . getScore . fst . tag

main = runEditor editor (foldl (+++) Empty . map fromString $
     ["This is the most amazing", "and funky", "editor ever"]
     :: JoinList (Score, Size) String)
