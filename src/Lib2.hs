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
        Right(rCmd) -> Right(rCmd) -- <--- continue case stairs


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
                else if cmd == "list"   then Right ListQuery
                else                    Left "Invalid command"


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

