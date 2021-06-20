{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes#-}
import Control.Applicative (ZipList(ZipList), getZipList, (<**>))

newtype List a = List [a] deriving (Functor, Eq, Show)

instance Applicative List where
  pure x = List [x, x]
  (List xs) <*> (List ys) = List $ xs <*> ys

{-

-- Interchange
>>> let fs = List [(+ 1), (+ 2)]
>>> let x = 1
>>> let leftInterchange  = fs         <*> pure x
>>> let rightInterchange = pure ($ x) <*> fs
>>> leftInterchange
>>> rightInterchange
>>> leftInterchange == rightInterchange
List [2,2,3,3]
List [2,3,2,3]
False

-}


(>$<) :: (a -> b) -> [a] -> [b]
f >$< la = getZipList $ f <$> ZipList la

(>*<) :: [a -> b] -> [a] -> [b]
lf >*< la = getZipList $ ZipList lf <*> ZipList la

{-

>>> let x1s = [1,2,3]
>>> let x2s = [4,5,6]
>>> let x3s = [7,8,9]
>>> let x4s = [10,11,12]

>>> (\a b -> 2*a+3*b) >$< x1s >*< x2s
[14,19,24]

>>> (\a b c -> 2*a+3*b+5*c) >$< x1s >*< x2s >*< x3s
[49,59,69]

>>> (\a b c d -> 2*a+3*b+5*c-4*d) >$< x1s >*< x2s >*< x3s >*< x4s
[9,15,21]

-}


divideList :: Fractional a => [a] -> a
divideList []     = 1
divideList (x:xs) = (/) x (divideList xs)

divideList' :: (Show a, Fractional a) => [a] -> (String, a)
divideList' []     = ("1.0", 1)
divideList' (x:xs) = (/) <$> ("<-" <> show x <> "/", x) <*> (divideList' xs)


{-

>>> divideList [3,4,5]
3.75

>>> divideList' [3,4,5]
("<-3.0/<-4.0/<-5.0/1.0",3.75)

-}

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
  fmap f (Arr2 g) = Arr2 $ \e1 e2 -> f $ g e1 e2

instance Functor (Arr3 e1 e2 e3) where
  fmap f (Arr3 g) = Arr3 $ \e1 e2 e3 -> f $ g e1 e2 e3

instance Applicative (Arr2 e1 e2) where
  pure x = Arr2 $ \_ _ -> x
  (<*>) (Arr2 f) (Arr2 g) = Arr2 $ \e1 e2 -> f e1 e2 $ g e1 e2

instance Applicative (Arr3 e1 e2 e3) where
  pure x = Arr3 $ \_ _ _ -> x
  (<*>) (Arr3 f) (Arr3 g) = Arr3 $ \e1 e2 e3 -> f e1 e2 e3 $ g e1 e2 e3

{-

>>> getArr2 (Arr2 (\x y z -> x+y-z) <*> Arr2 (*)) 2 3
-1

>>> getArr3 (Arr3 (\x y z w -> x+y+z-w) <*> Arr3 (\x y z -> x*y*z)) 2 3 4
-15

-}


{-

>>> (,) <$> "dog" <*> "cat"
[('d','c'),('d','a'),('d','t'),('o','c'),('o','a'),('o','t'),('g','c'),('g','a'),('g','t')]

>>> zip <*> tail $ [1, 2, 3, 4, 5]
[(1,2),(2,3),(3,4),(4,5)]

-}

infixl 4 <*?>
(<*?>) :: Applicative f => f a -> f (a -> b) -> f b
(<*?>) = flip (<*>)

exprMaybe :: (forall a b . Maybe a -> Maybe (a -> b) -> Maybe b) -> Maybe Int
exprMaybe op =
  let (<??>) = op
      infixl 4 <??>
  in Just 5 <??> Just (+2)

exprList :: (forall a b . [a] -> [a -> b] -> [b]) -> [Int]
exprList op =
  let (<??>) = op
      infixl 4 <??>
  in [1,2,3] <??> [(+3),(+4)]

exprZipList :: (forall a b . ZipList a -> ZipList (a -> b) -> ZipList b) -> ZipList Int
exprZipList op =
  let (<??>) = op
      infixl 4 <??>
  in ZipList [1,2] <??> ZipList [(+3),(+4)]

exprEither :: (forall a b . Either String a -> Either String (a -> b) -> Either String b) -> Either String Int
exprEither op =
  let (<??>) = op
      infixl 4 <??>
  in Left "AA" <??> Left "BB"

exprPair :: (forall a b . (String,a) -> (String,a -> b) -> (String,b)) -> (String,Int)
exprPair op =
  let (<??>) = op
      infixl 4 <??>
  in ("AA", 3) <??> ("B", (+1))

exprEnv :: (forall a b . (String -> a) -> (String -> (a -> b)) -> (String -> b)) -> (String -> Int)
exprEnv op =
  let (<??>) = op
      infixl 4 <??>
  in length <??> (\_ -> (+5))

{-



>>> exprMaybe (<**>)
>>> exprMaybe (<*?>)
>>> exprMaybe (<**>) == exprMaybe (<*?>)
Just 7
Just 7
True

>>> exprList (<**>)
>>> exprList (<*?>)
>>> exprList (<**>) == exprList (<*?>)
[4,5,5,6,6,7]
[4,5,6,5,6,7]
False

>>> exprZipList (<**>)
>>> exprZipList (<*?>)
>>> exprZipList (<**>) == exprZipList (<*?>)
ZipList {getZipList = [4,6]}
ZipList {getZipList = [4,6]}
True

>>> exprEither (<**>)
>>> exprEither (<*?>)
>>> exprEither (<**>) == exprEither (<*?>)
Left "AA"
Left "BB"
False

>>> exprPair (<**>)
>>> exprPair (<*?>)
>>> exprPair (<**>) == exprPair (<*?>)
("AAB",4)
("BAA",4)
False

>>> let arg = "Hello"
>>> exprEnv (<**>) arg
>>> exprEnv (<*?>) arg
>>> exprEnv (<**>) arg == exprEnv (<*?>) arg
10
10
True

-}
