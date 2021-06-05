{-# OPTIONS_GHC -Wall -Werror #-}

import Text.Read ( readMaybe )

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n xs = head xs : myTake (n - 1) (tail xs)

{-

>>> myTake 2 [1, 2, 3]
>>> myTake 4 [1, 2, 3]
[1,2]
Prelude.head: empty list

-}

myTakePM :: Int -> [a] -> [a]
myTakePM 0 _ = []
myTakePM _ [] = []
myTakePM n (x:xs) = x : myTakePM (n - 1) xs

{-

>>> myTakePM 2 [1, 2, 3]
>>> myTakePM 4 [1, 2, 3]
[1,2]
[1,2,3]

>>> 5 / 2
>>> 2 / 0
2.5
Infinity

>>> :t maximum
>>> maximum [] :: Int
maximum :: forall (t :: * -> *) a. (Foldable t, Ord a) => t a -> a
Prelude.maximum: empty list

>>> :t succ
>>> succ maxBound :: Int
succ :: forall a. Enum a => a -> a
Prelude.Enum.succ{Int}: tried to take `succ' of maxBound

>>> :t pred
>>> pred minBound :: Int
pred :: forall a. Enum a => a -> a
Prelude.Enum.pred{Int}: tried to take `pred' of minBound

>>> :t sum
>>> sum [1 ..] :: Int
sum :: forall (t :: * -> *) a. (Foldable t, Num a) => t a -> a
ProgressCancelledException

-}

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing 
maybeHead (x : _) = Just x

{-

>>> maybeHead [1]
>>> maybeHead [1 ..]
>>> maybeHead [] :: Maybe Int
Just 1
Just 1
Nothing

-}

myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer _ Nothing = Nothing
myTakeSafer n (Just xs) = (:) <$> maybeHead xs
                              <*> myTakeSafer (n - 1) (Just (tail xs))

{-

>>> myTakeSafer 3 (Just [1, 2, 3])
Just [1,2,3]

>>> myTakeSafer 6 (Just [1, 2, 3])
Nothing

>>> myTakeSafer 0 (Just []) :: Maybe [Int]
Just []

>>> myTakeSafer 6 (Just []) :: Maybe [Int]
Nothing

>>> myTakeSafer 6 Nothing :: Maybe [Int]
Nothing

>>> myTakeSafer 6 (Just [1 ..]) :: Maybe [Int]
Just [1,2,3,4,5,6]

-}

eitherHead :: [a] -> Either String a
eitherHead [] = Left "There is no head because the list is empty"
eitherHead (x : _) = Right x

intExample :: [Int]
intExample = [1, 2, 3]

intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: [Char]
charExample = "cat"

charExampleEmpty :: [Char]
charExampleEmpty = ""

{-

>>> eitherHead intExample
Right 1

>>> eitherHead intExampleEmpty
Left "There is no head because the list is empty"

>>> eitherHead charExample
Right 'c'

>>> eitherHead charExampleEmpty
Left "There is no head because the list is empty"

>>> (+ 1) <$> (eitherHead intExample)
Right 2

>>> (+1) <$> (eitherHead intExampleEmpty)
Left "There is no head because the list is empty"

>>> (+) <$> eitherHead intExample <*> eitherHead (tail intExample)
Right 3

-}

-- Q38.1

maybeToEither :: e -> Maybe r -> Either e r
maybeToEither err Nothing = Left err
maybeToEither _ (Just res) = Right res

flipEither :: Either a b -> Either b a
flipEither (Left err) = Right err
flipEither (Right val) = Left val

bothFailed :: Either e1 r -> Either e2 r -> (e1 -> e2 -> e) -> Either e r
bothFailed f s combineErrors = flipEither $ do
  e1 <- flipEither f
  e2 <- flipEither s
  return $ combineErrors e1 e2

addStrInts :: String -> String -> Either String Int
addStrInts xStr yStr = do
  _ <- bothFailed eitherX eitherY (\_ _ -> "Neither value can be parsed")
  x <- eitherX
  y <- eitherY
  return $ x + y
  where
    maybeX = readMaybe xStr :: Maybe Int
    maybeY = readMaybe yStr :: Maybe Int
    eitherX = maybeToEither "The first value cannot be parsed" maybeX
    eitherY = maybeToEither "The second value cannot be parsed" maybeY

{-

>>> addStrInts "100" "200"
Right 300

>>> addStrInts "dfsad" "200"
Left "The first value cannot be parsed"

>>> addStrInts "100" "sfgs"
Left "The second value cannot be parsed"

>>> addStrInts "dsnjla" "sfgs"
Left "Neither value can be parsed"

-}

safeSucc :: (Enum e, Ord e, Bounded e) => e -> Maybe e
safeSucc val | val == maxBound = Nothing
             | otherwise = Just $ succ val

{-

>>> safeSucc maxBound :: Maybe Int
Nothing

>>> safeSucc 10 :: Maybe Int
Just 11

-}

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_ : xs) = xs

{-

>>> safeTail [] :: [Int]
[]

>>> safeTail [10] :: [Int]
[]

>>> safeTail [10, 20, 30] :: [Int]
[20,30]

-}

safeLast :: [a] -> Either String a
safeLast [] = Left "No last element in the empty list"
safeLast as | isApproximatelyInfinite as = Left "List length is more than the maxBound of Int, probably it is infinite."
            | otherwise = Right $ last as
  where isApproximatelyInfinite = not . null . drop maxBound

{-

>>> safeLast [] :: Either String Int
Left "No last element in the empty list"

>>> safeLast [1, 2, 3]
Right 3

-- >>> safeLast [1 ..]

-}
