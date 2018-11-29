{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Monoid

newtype Score = Score Integer deriving (Eq, Show, Ord, Num)

getScore :: Score -> Integer
getScore (Score n) = n

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)
