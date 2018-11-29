{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Data.Ord

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

sortedDice :: Int -> Rand StdGen [DieValue]
sortedDice n = sortOn Down <$> dice n

attackerLosses as ds = length . filter id $ zipWith (<=) as ds
defenderLosses as ds = length . filter id $ zipWith (>) as ds

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = sortedDice (min 3 a) >>= \as ->
                           sortedDice (min 2 d) >>= \ds ->
                           let a' = a - attackerLosses as ds
                               d' = d - defenderLosses as ds
                           in  pure (Battlefield a' d')

invade :: Battlefield -> Rand StdGen Battlefield
invade (Battlefield a 0)         = pure $ Battlefield a 0
invade (Battlefield a n) | a < 2 = pure $ Battlefield a n
invade b                         = pure b >>= battle >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb = fmap prop . replicateM 1000 . invade
                where prop = (* 0.001) . fromIntegral . length
                                          . filter ((== 0) . defenders)
