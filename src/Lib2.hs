{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where
import qualified Data.Char as C
import qualified Data.List as L

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query = 
    AddQuery String |
    RemoveQuery String |
    ListQuery

-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _= False

instance Show Query where
    show (AddQuery a)       = show a
    show (RemoveQuery a)    = show a
    show ListQuery          = show "List:"

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery str = 
    case parseCmd str of
        Left(eCmd) -> Left(eCmd)
        Right(rCmd) -> 
			case parseBook rCmd of
				Left(eBook) -> Left(eBook)
				Right(rBook) -> Right(rBook) -- <-- continue stairs


parseCmd :: String -> Either String Query
parseCmd str =
        let
                cmd = L.takeWhile C.isAlpha str
                rest1 = drop (length cmd) str
                rest2 = L.takeWhile C.isSpace rest1
                rest = drop (length rest2) rest1
        in
                if      cmd == "add"    then Right (AddQuery rest)
                else if cmd == "remove" then Right (RemoveQuery rest)
                else if cmd == "list"   then Right (ListQuery)
                else                    Left "Invalid command"

parseBook :: Query -> Either String Query
parseBook ListQuery = Right ListQuery
parseBook (AddQuery str) = 
	case parseTitle str of
		Left(e1) -> Left(e1)
		Right(title, r1) -> Right(AddQuery title)
parseBook (RemoveQuery str) = Right (RemoveQuery str) -- <-- to implement

-- "Title example", 
parseTitle :: String -> Either String (String, String)
parseTitle str = 
	let
		titleStart = L.take 1 str
		rest1 = drop 1 str
		title = L.takeWhile (/= '\"') rest1
		rest2 = drop ((length title) + 2) rest1
		rest3 = L.takeWhile C.isSpace rest2
		rest = drop (length rest3) rest2
	in
		if titleStart == "\"" then Right (title, rest)
		else Left "Invalid Title syntax"


-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State = State [Query]
    deriving Show

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State []

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition (State old) q = 
    case q of
        ListQuery ->
            Right(Just (show (State old)), State old)
        AddQuery a ->
            let
                id = AddQuery ((show (length old + 1))++". "++a)
                new = State(old++[id])  
            in
                Right(Just (show new), new)

