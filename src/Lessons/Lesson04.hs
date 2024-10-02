{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lessons.Lesson04 () where
import qualified Data.Char as C
import qualified Data.List as L
import Data.Char (isAlpha)


type Parser a = String -> Either String (a, String)
-- >>> parseChar ' ' " dda"
-- Right (' ',"dda")
parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c s@(h:t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

-- >>> parseLetter "abc" 
-- Right ('a',"bc")
-- >>> parseLetter "* sa"
-- Left "Symbol * is not a letter"
parseLetter :: Parser Char
parseLetter [] = Left ("Cannot find any letter in an empty input")
parseLetter (h:t) = if C.isLetter h then Right (h,t) else Left ("Symbol "++[h]++" is not a letter")

-- >>> parseDigit "123"
-- Right ('1',"23")
parseDigit :: Parser Char
parseDigit [] = Left ("Cannot find any digit in an empty input")
parseDigit (h:t) = if C.isDigit h then Right (h,t) else Left ("Symbol "++[h]++" is not a digit")


-- >>> parseNumber "123gjh123"
-- Right (123,"gjh123")
parseNumber :: Parser Integer
parseNumber [] = Left ("Cannot parse number")
parseNumber str = 
	let 
		digits = L.takeWhile C.isDigit str
		rest = drop (length digits) str
	in
		case digits of
			[] -> Left ("not a number")
			_ -> Right (read digits :: Integer, rest)

-- >>> parseAlphaNum "p11c"
-- Right ('p',"11c")
parseAlphaNum :: Parser Char
parseAlphaNum = or2 parseLetter parseDigit

-- >>> and2 parseLetter parseLetter "hi"
-- Right (('h','i'),"")
and2 :: Parser a -> Parser b -> Parser (a,b)
and2 a b = \input -> 
	case a input of
		Right (v1, r1) ->
			case b r1 of
				Right (v2, r2) -> Right ((v1,v2), r2)
				Left e2 -> Left e2
		Left e1 -> Left e1

or2 :: Parser a -> Parser a -> Parser a
or2 a b = \input -> 
	case a input of
		Right r1 -> Right r1
		Left e1 ->
			case b input of
				Right r2 -> Right r2
				Left e2 -> Left (e1++", "++e2)


