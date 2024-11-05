{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
module Lessons.Lesson08 (Parser(..)) where

import Data.Char as C
type OldParser a = String -> Either String (a, String)
parseDigit' :: OldParser Char
parseDigit' [] = Left "Cannot find any digits in an empty input"
parseDigit' s@(h:t) = if C.isDigit h then Right (h, t) else Left (s ++ " does not start with a digit")
data Student = Student {
    name :: String,
    age :: Int
}
data Parser a = Parser {
    runParser :: String -> Either String (a, String)
}
-- >>> runParser parseDigit "123"
-- Right ('1',"23")
-- >>> runParser parseDigit "a123"
-- Left "a123 does not start with a digit"
parseDigit :: Parser Char
parseDigit = Parser $ \input ->
    case input of
        [] -> Left "Cannot find any digits in an empty input"
        s@(h:t) -> if C.isDigit h then Right (h, t) else Left (s ++ " does not start with a digit")
-- >>> runParser (fmap (\a -> [a,a]) parseDigit) "123" 
-- Right ("11","23")
-- >>> runParser (fmap (\a -> [a,a]) parseDigit) "s123" 
-- Left "s123 does not start with a digit"
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f functor = Parser $ \input ->
        case runParser functor input of
            Left e -> Left e
            Right (v, r) -> Right (f v, r)
instance Applicative Parser where
    pure :: a -> Parser a
    pure a = Parser $ \input -> Right (a, input)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    ff <*> fa = Parser $ \input ->
        case runParser ff input of
            Left e1 -> Left e1
            Right (f, r1) -> case runParser fa r1 of
                                Left e2 -> Left e2
                                Right (a, r2) -> Right (f a , r2)
parseTwoDigits :: Parser (Char, Char)
parseTwoDigits = (,) <$> parseDigit <*> parseDigit
-- >>> runParser parseThreeDigits ""
-- Left "Cannot find any digits in an empty input"
-- >>> runParser parseThreeDigits "1"
-- Left "Cannot find any digits in an empty input"
-- >>> runParser parseThreeDigits "123"
-- Right (('1','2','3'),"")
-- >>> runParser parseThreeDigits "1234"
-- Right (('1','2','3'),"4")
parseThreeDigits :: Parser (Char, Char, Char)
parseThreeDigits = (,,) <$> parseDigit <*> parseDigit <*> parseDigit
instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    ma >>= mf = Parser $ \input ->
        case runParser ma input of
            Left e1 -> Left e1
            Right (a, r1) -> case runParser (mf a) r1 of
                                Left e2 -> Left e2
                                Right (b, r2) -> Right (b, r2)
-- >>> runParser parseThreeDigits' "1234"
-- Right (('1','2','3'),"4")
parseThreeDigits' :: Parser (Char, Char, Char)
parseThreeDigits' = do
    a <- parseDigit
    b <- parseDigit
    c <- parseDigit
    return (a, b, c)

-- >>> Nothing <|> Just 5 <|> Just 6
-- Variable not in scope:
--   (<|>) :: t0_a1cqj[tau:1] -> Maybe a2_a1cqQ[tau:1] -> t_a1cqr[sk:1]
-- Variable not in scope:
--   (<|>)
--     :: Maybe a0_a1cqK[tau:1]
--        -> Maybe a1_a1cqM[tau:1] -> t0_a1cqj[tau:1]
