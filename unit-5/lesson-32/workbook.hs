import Control.Monad ( guard )
import Control.Applicative ( Alternative, empty )
import Data.Char ( toUpper )
import Data.List ( groupBy )

powersOfTwo :: Int -> [Int]
powersOfTwo n = do
  value <- [1 .. n]
  return $ 2 ^ value

{-

>>> powersOfTwo 10
[2,4,8,16,32,64,128,256,512,1024]

-}

powersOfTwoMap :: Int -> [Int]
powersOfTwoMap n = map (^2) [1 .. n]

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
  value <- [1 .. n]
  let powersOfTwo = 2 ^ value
  let powersOfThree = 3 ^ value
  return (powersOfTwo, powersOfThree)

{-

>>> powersOfTwoAndThree 5
[(2,3),(4,9),(8,27),(16,81),(32,243)]

-}

allEvenOdd :: Int -> [(Int, Int)]
allEvenOdd n = do
  evenValue <- [2, 4 .. n]
  oddValue <- [1, 3 .. n]
  return (evenValue, oddValue)


{-

>>> allEvenOdd 5
[(2,1),(2,3),(2,5),(4,1),(4,3),(4,5)]

>>> allEvenOdd 6
[(2,1),(2,3),(2,5),(4,1),(4,3),(4,5),(6,1),(6,3),(6,5)]

-}

valAndSquare :: Int -> [(Int, Int)]
valAndSquare n = do
  i <- [1 .. n]
  return (i, i ^ 2)

{-

>>> valAndSquare 10
[(1,1),(2,4),(3,9),(4,16),(5,25),(6,36),(7,49),(8,64),(9,81),(10,100)]

-}

evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1 .. n]
  guard $ even value
  return value

evensGuardWithoutDo :: Int -> [Int]
evensGuardWithoutDo n =
  [1 .. n] >>= (\value ->
    guard (even value) >> return value
    )


next :: (Monad m) => m a -> m b -> m b
next ma mb = ma >>= const mb

myGuard :: (Alternative f) => Bool -> f ()
myGuard True = pure ()
myGuard False = empty


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f la = do
  a <- la
  guard $ f a
  return a

powersOfTwoListComp :: Int -> [Int]
powersOfTwoListComp n = [2 ^ value | value <- [1 .. n]]

powersOfTwoAndThreeListComp :: Int -> [(Int, Int)]
powersOfTwoAndThreeListComp n = [(powerOfTwo, powerOfThree)
    | value <- [1 .. n]
    , let powerOfTwo = 2 ^ value
    , let powerOfThree = 3 ^ value
  ]

allEvenOddsListComp :: Int -> [(Int, Int)]
allEvenOddsListComp n = [(evenVal, oddVal) |
    evenVal <- [2, 4 .. n],
    oddVal <- [1, 3 .. n]
  ]

evensGuardListComp :: Int -> [Int]
evensGuardListComp n = [value | value <- [1 .. n], even value]

lastNames :: [String ]
lastNames = ["brown", "blue", "pink", "orange"]

capitalizeList :: [String] -> [String]
capitalizeList strings = ["Mr. " ++ (toUpper <$> fl) ++ r | s <- strings, let (fl, r) = splitAt 1 s]

{-

>>> capitalizeList lastNames
>>> capitalizeList [""]
>>> capitalizeList ["x"]
["Mr. Brown","Mr. Blue","Mr. Pink","Mr. Orange"]
["Mr. "]
["Mr. X"]

-}


-- Q32.1

datesForMonths :: [(Int, Int)]
datesForMonths = [(monthNumber, dayNumber) |
    monthNumber <- [1 .. 12],
    let numDaysInMonth = case monthNumber of
                           2 -> 28
                           _ | monthNumber <= 7 && odd monthNumber -> 31
                             | monthNumber <= 7 -> 30 -- even month before July
                             | monthNumber > 7 && odd monthNumber -> 30
                             | otherwise -> 31,
    dayNumber <- [1 .. numDaysInMonth]
  ]

maxDayForMonths :: [(Int, Int)]
maxDayForMonths = do
  monthDays <- groupBy (\(month, _) (prevMonth, _) -> month == prevMonth) datesForMonths
  let days = snd <$> monthDays
  guard $ not (null monthDays)
  return (fst $ head monthDays, maximum days)

{-

>>> maxDayForMonths
[(1,31),(2,28),(3,31),(4,30),(5,31),(6,30),(7,31),(8,31),(9,30),(10,31),(11,30),(12,31)]

-}


-- Q32.2

datesForMonthsDo :: [(Int, Int)]
datesForMonthsDo = do
  monthNumber <- [1 .. 12]
  let numDaysInMonth = case monthNumber of
                           2 -> 28
                           _ | monthNumber <= 7 && odd monthNumber -> 31
                             | monthNumber <= 7 -> 30 -- even month before July
                             | monthNumber > 7 && odd monthNumber -> 30
                             | otherwise -> 31
  dayNumber <- [1 .. numDaysInMonth]
  return (monthNumber, dayNumber)

datesForMonthsDesugared :: [(Int, Int)]
datesForMonthsDesugared = [1 .. 12] >>=
  (\monthNumber ->
    (\numDaysInMonth ->
        [1 .. numDaysInMonth] >>= (\dayNumber -> return (monthNumber, dayNumber))
      ) (case monthNumber of
                           2 -> 28
                           _ | monthNumber <= 7 && odd monthNumber -> 31
                             | monthNumber <= 7 -> 30 -- even month before July
                             | monthNumber > 7 && odd monthNumber -> 30
                             | otherwise -> 31)
    )
