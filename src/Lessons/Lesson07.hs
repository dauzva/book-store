{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Lessons.Lesson07 () where

---------- Cartesian Product (Dekarto Sandauga)
-- >>> cp
-- [(1,'a'),(1,'a'),(1,'b'),(1,'b'),(1,'c'),(1,'c'),(2,'a'),(2,'a'),(2,'b'),(2,'b'),(2,'c'),(2,'c'),(3,'a'),(3,'a'),(3,'b'),(3,'b'),(3,'c'),(3,'c'),(4,'a'),(4,'a'),(4,'b'),(4,'b'),(4,'c'),(4,'c'),(5,'a'),(5,'a'),(5,'b'),(5,'b'),(5,'c'),(5,'c')]
cp :: [(Integer, Char)]
cp = [(a, b) | a <- [1..5], b <- ['a'..'c'], c <- [True, False]]


-- >>> cpm
-- [(1,'a'),(1,'a'),(1,'b'),(1,'b'),(1,'c'),(1,'c'),(2,'a'),(2,'a'),(2,'b'),(2,'b'),(2,'c'),(2,'c'),(3,'a'),(3,'a'),(3,'b'),(3,'b'),(3,'c'),(3,'c'),(4,'a'),(4,'a'),(4,'b'),(4,'b'),(4,'c'),(4,'c'),(5,'a'),(5,'a'),(5,'b'),(5,'b'),(5,'c'),(5,'c')]
cpm :: [(Integer, Char)]
cpm = do
	a <- [1..5]
	b <- ['a'..'c']
	c <- [True, False]
	return (a,b)

-- >>> cpm'
-- []
cpm' :: [(Integer, Char)]
cpm' = do
	a <- [] -- if no value given then there is no calculation
	b <- ['a'..'c']
	c <- [True, False]
	return (a,b)

-- >>> cpm''
-- []
cpm'' :: [(Integer, Char)]
cpm'' = do
	a <- [1..5]
	b <- ['a'..'c']
	c <- []
	return (a,b)

-- >>> mm
-- Just 42
mm :: Maybe Integer
mm = do
	a <- Just 42
	b <- Just 'a'
	return a

-- >>> mm'
-- Nothing
mm' :: Maybe Integer
mm' = do
	a <- Just 42
	b <- Nothing -- bad value prevents calculation
	return a

-- >>> mm''
-- Just 43
mm'' :: Maybe Integer
mm'' = do
	a <- Just 42
	b <- Just "labas"
	return $ a+1

------------- Monadic Lists
--- example of map
-- >>> ml
-- [2,3,4]
ml :: [Integer]
ml = do
	a <- [1,2,3]
	return $ a+1

-- >>> em
-- Right (42,21)
em :: Either String (Int, Int)
em = do
	a <- Right 42
	b <- Right 21
	return (a,b)

-- >>> em'
-- Left "no"
em' :: Either String Int
em' = do
	c <- Left "no" -- Left is a bad value and is returned
	a <- Right 42
	b <- Left "labas"
	return a

-- >>> [1..10] >>= (\a -> return $ a^2)
-- [1,4,9,16,25,36,49,64,81,100]

-- >>> c 
-- [1,4,9,16,25,36,49,64,81,100]
c :: [Integer]
c = do
    a <- [1..10]
    return $ a^2

---------- Functors
-- >>> fi
-- [2,3,4]
fi :: [Integer]
fi = fmap (\a -> a+1) [1,2,3]

-- >>> fm
-- Just 11
fm :: Maybe Integer
fm = fmap (\a -> a+1) $ Just 10

-- >>> addMaybe (Just 1) (Just 4) (Just 2)
-- Just 7
addMaybe :: Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Integer
addMaybe a b c = (\x y z -> x+y+z) <$> a <*> b <*> c
