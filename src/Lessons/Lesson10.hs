{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Lessons.Lesson10 () where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Applicative (Alternative(some))


type Parser a = ExceptT String (State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)

-- >>> parse (parseChar 'a') "asdgf"
-- (Right 'a',"sdgf")
parseChar :: Char -> Parser Char
parseChar a = do
	input <- lift get
	case input of
		[] -> throwE "Empty"
		(h:t) -> if h == a
			then lift $ put t >> return h
			else throwE $ a:" is not found"
	return 'a' 
	

------------------------------------------------------
-- >>> parse (parseTwoAs) "a     avasd"
-- (Right (),"vasd")
parseTwoAs :: Parser ()
parseTwoAs = do
	_ <- parseChar 'a'
	_ <- some $ parseChar ' '
	_ <- parseChar 'a'
	return ()

-- >>> parse (parseTwoAs') "a     avasd"
-- (Right (),"vasd")
parseTwoAs' :: Parser ()
parseTwoAs' =
	(\ _ _ _ -> ()) <$> (parseChar 'a') <*> (some $ parseChar ' ') <*> (parseChar 'a')

-- >>> parse (parseTwoAs'') "a     avasd"
-- (Right (),"vasd")
parseTwoAs'' :: Parser ()
parseTwoAs'' = (parseChar 'a') >> (some $ parseChar ' ') >> parseChar 'a' >> return ()

-----------------------------------------------------------

type Weird a = ExceptT String (StateT Int IO) a



weird :: Weird Double
weird = do
	lift $ lift $ putStrLn "What is your name?"
	input <- lift $ lift getLine
	lift $ put $ length input 
	return 3.14


weird' :: Weird Double
weird' = do
	liftIO $ putStrLn "What is your name?" 		-- liftIO instead of lift
	input <- lift $ lift getLine
	lift $ put $ length input 
	return 3.14

-- Foldable - reduces data strucure into 1 value
-- Traversable - Foldable + Functors

-- >>> sequence [Just 1, Just 5]
-- Just [1,5]
-- >>> sequence [Just 1, Nothing, Just 5]
-- Nothing


-- >>> mapM (\a -> a) [Just 1, Just 5]
-- Just [1,5]
-- >>> mapM (\a -> a) [Just 1, Nothing, Just 5]
-- Nothing


-- >>> mapM_ (\a -> a) [Just 1, Just 5]
-- Just ()
---- no result
-- >>> mapM_ (\a -> a) [Just 1, Nothing, Just 5]
-- Nothing

