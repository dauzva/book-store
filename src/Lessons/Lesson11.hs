{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Lessons.Lesson11 () where

--import Control.Monad.Free

data Expr = Lit Integer
		| Add Expr Expr
		| Neg Expr
		deriving Show

--lit :: Integer -> Expr
--lit i = Lit i 

--prog1 :: Expr 
--prog1 = lit 4

prog :: Expr 
prog = Neg (Add (Lit 5) (Lit 6))


-- >>> eval prog
-- -11
eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Neg a) = - (eval a)

-- >>> print' prog
-- "-(5+6)"
print' :: Expr -> String
print' (Lit a) = show a
print' (Add e1 e2) =  show (eval e1) ++ "+" ++ show (eval e2)
print' (Neg a) = "-("++ (print' a)++")"


------------- Tagless Final ---------------

class Expression repr where
	lit :: Int -> repr
	add :: repr -> repr -> repr
	neg :: repr -> repr
	

instance Expression Int where
	lit :: Int -> Int
	lit a = a
	add :: Int -> Int -> Int
	add e1 e2 = e1 + e2
	neg :: Int -> Int
	neg a = -a

instance Expression String where
	lit :: Int -> String
	lit = show
	add :: String -> String -> String
	add e1 e2 = e1 ++"+"++ e2
	neg :: String -> String
	neg a = "-("++ a ++")"


-- >>> prog2
-- -11
prog2 :: Int
prog2 = neg (add (lit 5) (lit 6))

-- >>> prog3
-- "-(5+6)"
prog3 :: String
prog3 = neg (add (lit 5) (lit 6))



---------------- Generics ------------------------------

data GExpr a where
	GInt :: Integer -> GExpr Integer
	GBool :: Bool -> GExpr Bool
	GAdd :: GExpr Integer -> GExpr Integer -> GExpr Integer
	GAnd :: GExpr Bool -> GExpr Bool -> GExpr Bool
	GEq :: GExpr Integer -> GExpr Integer -> GExpr Bool


-- >>> evalG prog4
-- True
prog4 :: GExpr Bool
prog4 = GEq (GAdd (GInt 4) (GInt 5)) (GInt 9)

evalG :: GExpr a -> a
evalG (GInt i) = i
evalG (GBool b) = b
evalG (GAdd e1 e2) = (evalG e1) + (evalG e2)
evalG (GAnd e1 e2) = (evalG e1) && (evalG e2)
evalG (GEq e1 e2) = (evalG e1) == (evalG e2)

data Ops = Eval Expr | Apply (Int -> Expr)

prog5 :: [Ops]
prog5 = [
		Eval prog,
		Eval prog,
		Apply (\i -> prog)
	]

