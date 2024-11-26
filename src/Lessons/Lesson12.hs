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
{-# LANGUAGE DeriveFunctor #-}
module Lessons.Lesson12 () where
import Control.Monad.Free
import Data.IORef (newIORef)
import Control.Monad.Trans.State.Strict(State, StateT, get, put, runState, runStateT)
import GHC.IORef (IORef(IORef))


data MyDomainAlgebra next	= Load (() -> next)
							| Add Int (() -> next)
							| Dump (String -> next)
							| Save (() -> next)


-- >>> id 4
type MyDomain = Free MyDomainAlgebra
load :: MyDomain ()
load = liftF $ Load id

add :: Int -> MyDomain ()
add i = liftF $ Add i id

dump :: MyDomain String
dump = liftF $ Dump id

save :: MyDomain (String, String)
save = liftF $ Save id

program :: MyDomain (String, String)
program = do
	load
	b <- dump
	add (1 + 1)
	add 10
	a <- dump
	save
	return (b,a)


runIOLog :: MyDomain a -> IO a
runIOLog (Pure a) = return a
runIOLog (Free step) = do
	next <- runStep step
	runIOLog next
	where 
		runStep :: MyDomainAlgebra a -> IO a
		runStep (Load next) = do
			putStrLn "LOAD"
			return $ next ()
		runStep (Add i next) = do
			putStrLn "ADD" ++ show i 
			return $ next ()
		runStep (Dump next) = do
			putStrLn "DUMP"
			return $ next "fake State"
		runStep (Save next) = do
			putStrLn "SAVE"
			return # next ()

runWithState :: MyDomain a -> State (Int, String) a
runWithState (Pure a) = return a
runWithState (Free step) = do
	next <- runStep step
	runWithState next
	where 
		runStep :: MyDomainAlgebra a -> State (Int, String) a
		runStep (Load next) = do
			(_, f) <- get
			put (read f, f)
			return $ next ()
		runStep (Add i next) = do
			(s, f) <- get
			put (s + i, f) 
			return $ next ()
		runStep (Dump next) = do
			(_, f) <- get
			return $ next f
		runStep (Save next) = do
			(s, _) <- get
			put (s, show s)
			return # next ()


runIO :: MyDomain a -> IO a
runIO p = do
	v <- newIORef 0
	runIO' v p
	where
		runIO' :: IORef Int -> MyDomain a -> IO a
		runIO' _ (Pure a) -> return a
		runIO' v (Free step) -> do
			next <- runStep step
			runIO' v next
		runStep :: IORef Int -> MyDomainAlgebra a -> IO a
		runStep v (Load next) = do
			a <- S.
			return $ next ()
		runStep v (Add i next) = do
			putStrLn "ADD" ++ show i 
			return $ next ()
		runStep v (Dump next) = do
			putStrLn "DUMP"
			return $ next "fake State"
		runStep v (Save next) = do
			putStrLn "SAVE"
			return # next ()