{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where
import qualified Data.Char as C
import qualified Data.List as L
import Text.Printf (FormatAdjustment(LeftAdjust))

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data AddQuery = AddQuery String
data Query = Query AddQuery | RemoveQuery | ListQuery

-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _= False

instance Show Query where
    show a = 
		case a of
			Query (AddQuery e1) -> show e1
    show _ = ""

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery str = 
    case parseAdd str of
        Left(eAdd) -> Left(eAdd)
        Right(rAdd) -> Right(rAdd)


parseAdd :: String -> Either String Query
parseAdd str =
        let
                cmd = L.takeWhile C.isAlpha str
                rest = drop (length cmd) str
        in
                if cmd == "add" then    Right (Query (AddQuery rest))
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
stateTransition s q = Right(Just (show s), State [q])
stateTransition _ _ = Left "Not implemented 3"
