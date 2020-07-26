import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

file1 :: [(Int,Double)]
file1 = [ (1, 200.1), (2, 199.5), (3, 199.4)
        , (4, 198.9), (5, 199.0), (6, 200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9)
        ]

file2 :: [(Int,Double)]
file2 = [ (11, 201.6), (12, 201.5), (13, 201.5)
        , (14, 203.5), (15, 204.9), (16, 207.1)
        , (18, 210.5), (20, 208.8)
        ]

file3 :: [(Int,Double)]
file3 = [ (10, 201.2), (11, 201.6), (12, 201.5)
        , (13, 201.5), (14, 203.5), (17, 210.5)
        , (24, 215.1), (25, 218.7)
        ]

file4 :: [(Int,Double)]
file4 = [ (26, 219.8), (27, 220.5), (28, 223.8)
        , (29, 222.8), (30, 223.8), (31, 221.7)
        , (32, 222.3), (33, 220.8), (34, 219.4)
        , (35, 220.1), (36, 220.6)
         ]

data TS a = TS [Int] [Maybe a]

showTVPair :: Show a => Int -> Maybe a -> String
showTVPair time (Just value) = mconcat [show time, "\t|\t", show value, "\n"]
showTVPair time Nothing = mconcat [show time, "\t|\tNA\n"]

instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows
    where rows = zipWith showTVPair times values

createTS :: [Int] -> [a] -> TS a
createTS ts as = TS completeTimes valuesForTimes
  where completeTimes = [minimum ts .. maximum ts]
        timesValues = Map.fromList (zip ts as)
        valuesForTimes = map (`Map.lookup` timesValues) completeTimes

fileToTS :: [(Int, a)] -> TS a
fileToTS xs = createTS times values
  where (times, values) = unzip xs


ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair m (_, Nothing) = m
insertMaybePair m (key, Just value) = Map.insert key value m

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
  where bothTimes      = mconcat [t1, t2]
        completeTimes  = [minimum bothTimes .. maximum bothTimes]
        tvMap          = foldl insertMaybePair Map.empty (zip t1 v1)
        updatedMap     = foldl insertMaybePair tvMap (zip t2 v2)
        combinedValues = map (`Map.lookup` updatedMap) completeTimes

instance Semigroup (TS a) where
  (<>) = combineTS

instance Monoid (TS a) where
  mempty = TS [] []

tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

mean :: (Real a) => [a] -> Double
mean xs = total / count
  where total = (realToFrac . sum) xs
        count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values)
  | all (== Nothing ) values = Nothing
  | otherwise = Just avg
  where justVals = filter isJust values
        cleanVals = map fromJust justVals
        avg = mean cleanVals

type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare f = newFunc
  where newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)
        newFunc (_, Nothing) tsEntry = tsEntry
        newFunc tsEntry (_, Nothing) = tsEntry
        newFunc tsEntry1@(i1, Just v1) tsEntry2@(i2, Just v2)
          | f v1 v2 == v1 = tsEntry1
          | otherwise = tsEntry2

compareTS :: Eq a => CompareFunc a -> TS a -> Maybe (Int, Maybe a)
compareTS _ (TS [] []) = Nothing
compareTS f (TS times values)
  | all (== Nothing) values = Nothing
  | otherwise = Just res
  where pairs = zip times values
        res = foldl (makeTSCompare f) (0, Nothing) pairs

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max


diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just x) (Just y) = Just (x - y)

diffTS :: Num a => TS a -> TS a
diffTS e@(TS [] []) = e
diffTS (TS times values) = TS times (Nothing : diffValues)
  where shiftedValues = tail values
        diffValues = zipWith diffPair shiftedValues values


meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe vals
  | any (== Nothing) vals = Nothing
  | otherwise = Just avg
  where avg = mean . map fromJust $ vals

movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] n = []
movingAvg vals n
  | length nextVals == n = meanMaybe nextVals : movingAvg restVals n
  | otherwise = []
  where nextVals = take n vals
        restVals = tail vals

movingAverageTS :: (Real a) => TS a -> Int -> TS Double
movingAverageTS (TS [] []) _ = TS [] []
movingAverageTS (TS times values) n = TS times smoothedValues
  where ma = movingAvg values n
        nothings = replicate (n `div` 2) Nothing
        smoothedValues = mconcat [nothings, ma, nothings]
