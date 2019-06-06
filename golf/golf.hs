module Golf where

import Data.List

mask :: [Bool] -> [a] -> [a]
mask bs = map snd . filter fst . zip bs

skips :: [a] -> [[a]]
skips xs = (\n -> mask (map (== n) $ cycle [1..n]) xs) <$> [1..length xs]

localMaxima :: [Integer] -> [Integer]
localMaxima xs = mask (zipWith (&&) (zipWith (>) xs (drop 1 xs) ++ [False])
                       $ False : zipWith (>) (drop 1 xs) xs) xs

histogram :: [Integer] -> IO ()
histogram i = (mapM_ putStrLn . stars $ ((\n -> length . filter (== n))
                                    <$> [0..9]) <*> [i]) >> putStrLn ['0'..'9']
             where stars ns = transpose . map (starIt (maximum ns)) $ ns
                   starIt t m = replicate (t-m) ' ' ++ replicate m '▓'
