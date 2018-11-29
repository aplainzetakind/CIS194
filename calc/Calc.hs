{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified StackVM as S
import qualified Data.Map as M

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | VVar String deriving (Eq, Show)

instance HasVars VarExprT where
  var = VVar

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit       = const . Just
  add f g m = (+) <$> f m <*> g m
  mul f g m = (*) <$> f m <*> g m

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax n) (MinMax m) = MinMax $ max n m
  mul (MinMax n) (MinMax m) = MinMax $ min n m

instance Expr Mod7 where
  lit = Mod7 . flip mod 7
  add (Mod7 n) (Mod7 m) = Mod7 $ mod (n + m) 7
  mul (Mod7 n) (Mod7 m) = Mod7 $ mod (n * m) 7

instance Expr S.Program where
  lit x = [S.PushI x]
  add p q = q ++ p ++ [S.Add]
  mul p q = q ++ p ++ [S.Mul]

eval :: ExprT -> Integer
eval (Lit n)   = n
eval (Add e f) = eval e + eval f
eval (Mul e f) = eval e * eval f

evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit Add Mul s

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer)
                                -> Maybe Integer
withVars vs epr = epr $ M.fromList vs
