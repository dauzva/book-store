{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lessons.Lesson05 () where

import Lessons.Lesson04 (Parser, and2, parseChar, parseAlphaNum, parseNumber)

--listOfNumbers := number (,number)*

--parseListOfNumbers :: Parser [Integer]
--parseListOfNumbers = and2 (:)
--						parseNumber
--						(many (and2 (\_ b ->b) (parseChar ',') parseNumber))
--
--many :: Parser a -> Parser [a]
--many p = many' p []
--	where
--		many' p' acc = \input ->-
--			case p' input of
--				Left e1 -> Right(acc, input)
--				Right (v1, r1) -> many p' (acc++[v1]) r1  

--parseListOfNumbers :: Parser (Integer, Integer)
--parseListOfNumbers = and2 (\a b -> (a,b)) parseNumber (and2 (\_ b ->b) (parseChar ',') parseNumber)

---------- RECORDS -----------------

-- >>> Person "Petras" 45
-- Person "Petras" 45
data Person = Person String Integer
	deriving Show

-- >>> Person' "Petras" 45
-- Person' {name = "Petras", age = 45}
data Person' = Person' {
	name :: String,
	age :: Integer
} deriving Show

-- >>> me {age=13}
-- Person' {name = "Petras", age = 13}
-- >>> age me {age=13}
-- 13
me :: Person'
me = Person' "Petras" 45
