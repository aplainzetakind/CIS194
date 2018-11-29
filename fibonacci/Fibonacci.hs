{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = a :- (Stream a) deriving Eq

infixr 5 :-

instance Show a => Show (Stream a) where
  show = show . streamTake 50

streamTake :: Integer -> Stream a -> [a]
streamTake n | n <= 0 = const []
        | otherwise = \(y :- ys) -> y : streamTake (n - 1) ys

streamRepeat :: a -> Stream a
streamRepeat y = y :- streamRepeat y

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (y :- ys) = f y :- streamMap f ys

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f y = y :- streamFromSeed f (f y)

nats :: Stream Integer
nats = streamFromSeed succ 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (s :- ss) ts = s :- interleaveStreams ts ss

ruler :: Stream Integer
ruler = foldr (interleaveStreams . streamRepeat) (streamRepeat 0) [0..]

x :: Stream Integer
x = 0 :- 1 :- streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = n :- streamRepeat 0
  negate (s :- ss) = negate s :- negate ss
  (+) (s :- ss) (t :- ts) = (s + t) :- (+) ss ts
  (*) (s :- ss) r@(t :- ts) = (s * t) :- (+) (streamMap (* s) ts) (ss * r)

instance Fractional (Stream Integer) where
  (/) p@(s :- ss) r@(t :- ts) = (s `div` t) :-
                                        streamMap (`div` t) (ss - (p / r) * ts)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^(2::Int))

data Matrix = M Integer Integer Integer Integer deriving (Eq, Show)

instance Num Matrix where
  (*) (M a b c d) (M a' b' c' d') = M w x y z
            where w = a * a' + b * c'
                  x = a * b' + b * d'
                  y = c * a' + d * c'
                  z = c * b' + d * d'

getFirstEntry :: Matrix -> Integer
getFirstEntry (M x _ _ _) = x

f :: Matrix
f = M 1 1 1 0
