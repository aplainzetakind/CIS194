module Golf where

import Data.List

mask :: ([a] -> [Bool]) -> [a] -> [a]
mask f xs = map snd . filter fst $ zip (f xs) xs

skips :: [a] -> [[a]]
skips x = (\n -> mask (const $  map ((== 0) . (`mod` n)) [1..l]))
                    <$> [1..l] <*> [x]
              where l = length x

localMaxima :: [Integer] -> [Integer]
localMaxima = mask (\xs -> zipWith (&&) (zipWith (>) xs (drop 1 xs) ++ [False])
                 $ False : zipWith (>) (drop 1 xs) xs)

histogram :: [Integer] -> IO ()
histogram i = (mapM_ putStrLn . stars $ ((\n -> length . filter (== n))
                                    <$> [0..9]) <*> [i]) >> putStrLn ['0'..'9']
             where stars ns = (transpose . map (starIt (maximum ns))) ns
                   starIt t m = replicate (t-m) ' ' ++ replicate m '▓'
