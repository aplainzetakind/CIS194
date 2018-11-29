{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.List (intercalate, sort)
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons e l = GL (e : listList l) (empFun e + listFun l)

moreFun :: GuestList -> GuestList -> GuestList
moreFun l1 l2 | l1 < l2   = l2
              | otherwise = l1

treeFold :: Monoid b => (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a ts) = f a $ map (treeFold f) ts

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e [] = (GL [e] (empFun e), GL [] 0)
nextLevel e l  = (,) <$> (glCons e . foldMap snd) <*> (foldMap fst) $ l

-- Not used, just another thing to fold with
names :: Employee -> [String] -> String
names e [] = empName e
names e l = intercalate "\n" $ empName e : l

glToString :: GuestList -> String
glToString (GL emps f) = unlines $ ("Total fun: " ++ show f)
                                   : (sort . map empName) emps

maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . treeFold nextLevel

main :: IO ()
main = do
    file <- readFile "company.txt"
    putStr . glToString . maxFun $ read file
