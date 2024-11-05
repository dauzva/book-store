{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use lambda-case" #-}
module Lessons.Lesson09 () where
import Control.Applicative
import Lessons.Lesson08 (Parser(..))
-- Arbitrary = random

--data IntOrString = Int' Integer | String' String
--instance Arbitrary IntOrString where
--	arbitrary :: Gen IntOrString
--	arbitrary =
--		oneof [fmap Int' arbitrary, fmap String' arbitrary]

instance Alternative Parser where
	empty :: Parser a
	empty = Parser $ \input -> Left $ "cannot parse "++input
	(<|>) :: Parser a -> Parser a -> Parser a
	p1 <|> p2 = Parser $ \input ->
		case (runParser p1 input) of
			Right r1 -> Right r1
			Left e1 -> case (runParser p2 input) of
				Right r2 -> Right r2
				Left e2 -> Left $ e1 ++ ", " ++ e2

parseChar :: Char -> Parser Char
parseChar c = Parser $ \input ->
	case input of
		[] -> Left ("Cannot find " ++ [c] ++ " in an empty input")
		s@(h:t) -> if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

-- >>> runParser parseA "aka"
-- Right ('a',"ka")
parseA :: Parser Char
parseA = (parseChar 'a') <|> (parseChar 'A')

-- runState stateful "initial" -> (7, "initial")
-- runState stateful "initial" -> (7, "new state")
--stateful :: State String Int
--stateful = do
--	value <- get
--	let l = lenght value
--	put "new state"
--	return l

-- runState combined "initial" -> ((7,9), "new state")
--combined :: State String Int
--combined = do
--	a <- stateful
--	b <- stateful
--	return (a,b)

-- s = type of state
-- a = type of computation
data State' s a = State' {
	runState' :: s -> (a,s)
}


get' :: State' a a
get' = State' $ \state -> (state, state)

put' :: s -> State' s ()
put' newstate = State' $ \_ -> ((), newstate)


instance Functor (State' s) where
	fmap :: (a -> b) -> State' s a -> State' s b
	fmap f functor = State' $ \input -> 
		case runState' functor input of
			(a, ns) -> (f a, ns)

-- >>> runState' (fmap (\_ -> 5) (put' "state 1")) "init"
-- (5,"state 1")

-- >>> runState' (put' "state 1") "init"
-- ((),"state 1")

-- >>> runState' (get') "init"
-- ("init","init")
