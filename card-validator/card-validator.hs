module CardValidator where

import System.Environment
import Data.List (unfoldr)
import Data.Tuple (swap)
import Control.Monad

toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n <= 0 = []
              | n < 10 = [n]
              | otherwise = r : toDigitsRev s
                  where (s,r) = n `divMod` 10

toDigitsRev' :: Integer -> [Integer]
toDigitsRev' = unfoldr $ fmap (swap . (`divMod` 10)) . mfilter (>0) . pure

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) $ cycle [1,2]

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigitsRev

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigitsRev

main :: IO ()
main = do
    arg <- fmap head getArgs
    if validate . read $ arg
      then putStrLn "This is a valid credit card number"
      else putStrLn "This is not a valid credit card number"
