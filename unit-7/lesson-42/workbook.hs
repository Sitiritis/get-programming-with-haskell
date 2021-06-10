import Control.Monad
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST

import Data.STRef

aLargeList :: [Int]
aLargeList = [1 .. 10000000]

-- aLargeArray :: UArray Int Int
-- aLargeArray = array (0, 9999999) []

aLargeListDoubled :: [Int]
aLargeListDoubled = map (*2) aLargeList

qcArray :: UArray Int Bool
qcArray = array (0, 4) $ zip [0 .. 1] $ repeat True

beansInBuckets :: UArray Int Int
beansInBuckets = array (minIdx, maxIdx) $ zip [minIdx, maxIdx] $ repeat 0
  where minIdx = 0
        maxIdx = 3

updatedBiB :: UArray Int Int
updatedBiB = beansInBuckets // [(1, 5), (3, 6)]

updatedBiB2 :: UArray Int Int
updatedBiB2 = accum (+) updatedBiB $ zip [0 .. 3] $ repeat 2

updatedBiDoubled :: UArray Int Int
updatedBiDoubled = accum (*) updatedBiB $ zip [0 .. 3] $ repeat 2

listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
  let end = length vals - 1
  stArray <- newArray (0, end) 0
  forM_ (zip [0 .. ] vals) $
    uncurry (writeArray stArray)

    -- \(i, v) ->
    -- writeArray stArray i v

  -- forM_ [0 .. end] $ \i -> do
  --   let val = vals !! i
  --   writeArray stArray i val

  return stArray

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals

-- listToSTUArray :: [Int] -> STUArray s Int Int
-- listToSTUArray vals = runST $ listToSTUArray vals


-- | We have no way to extract ST ref here, because ST and STRef are parametrized with the same s, but
-- | runST requires to have different s-es for ST and STRef
-- testST :: ST s (STRef s Int)
-- testST = newSTRef (15 :: Int)

-- runTestST :: STRef s Int
-- runTestST = runST $ testST

-- | We would be able to runST if the type signature of testST would be such, but the standard API does not
-- | allow us to retrieve such value, genius :)
-- testST :: ST s (STRef s1 Int)
-- testST = _

-- runTestST :: STRef s Int
-- runTestST = runST $ testST

{-

>>> listToUArray [1, 2, 3]
array (0,2) [(0,1),(1,2),(2,3)]

-}

swapST :: (a, b) -> (b, a)
swapST (x, y) = runST $ do
  x' <- newSTRef y
  y' <- newSTRef x
  writeSTRef x' y
  writeSTRef y' x
  xFinal <- readSTRef x'
  yFinal <- readSTRef y'
  return (xFinal, yFinal)

myData :: UArray Int Int
myData = listArray (0, datLen) dat
  where
    dat = [7, 6, 4, 8, 10, 2]
    datLen = length dat

myData' :: UArray Int Int
myData' = listToUArray [7, 6, 4, 8, 10, 2]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort arr = runSTUArray $ do
  stArr <- thaw arr :: ST s (STUArray s Int Int)
  let end = snd . bounds $ arr
  forM_ [1 .. end] $ \i -> do
    forM_ [0 .. end - i] $ \j -> do
      val <- readArray stArr j
      nextVal <- readArray stArr (j + 1)
      when (val > nextVal) $ do
        writeArray stArr j nextVal
        writeArray stArr (j + 1) val
  return stArr

{-

>>> myData
>>> bubbleSort myData
>>> myData
array (0,6) [(0,7),(1,6),(2,4),(3,8),(4,10),(5,2),(6,0)]
array (0,6) [(0,0),(1,2),(2,4),(3,6),(4,7),(5,8),(6,10)]
array (0,6) [(0,7),(1,6),(2,4),(3,8),(4,10),(5,2),(6,0)]

-}

-- Q42.1

crossover :: UArray Int Int -> UArray Int Int -> Int -> UArray Int Int
crossover left right cutoff = runSTUArray $ do
  res <- newArray (0, resLastIdx) 0 :: ST s (STUArray s Int Int)
  forM_ [0 .. resLastIdx] $ \i -> do
    let val = if i < actualCutoff then left ! i
              else right ! i
    writeArray res i val
  return res
  where
    (leftMinBnd, leftMaxBnd) = bounds left
    leftLen = leftMaxBnd - leftMinBnd + 1
    (rightMinBnd, rightMaxBnd) = bounds right
    rightLen = rightMaxBnd - rightMinBnd + 1
    resLen = min leftLen rightLen
    resLastIdx = resLen - 1
    actualCutoff = min cutoff resLen

sampleLeft :: UArray Int Int
sampleLeft = listArray (0, 4) [1, 1, 1, 1, 1]

sampleRight :: UArray Int Int
sampleRight = listArray (0, 4) [0, 0, 0, 0, 0]

{-

>>> crossover sampleLeft sampleRight 3
array (0,4) [(0,1),(1,1),(2,1),(3,0),(4,0)]

-}

-- Q42.2

replaceZeroes :: UArray Int Int -> Int -> UArray Int Int
replaceZeroes arr val = runSTUArray $ do
  a <- thaw arr :: ST s (STUArray s Int Int)
  let (lowerBnd, upperBnd) = bounds arr
  forM_ [lowerBnd .. upperBnd] $ \i -> do
    e <- readArray a i
    when (e == 0) $ do
      writeArray a i val
  return a

sampleArrWithZeroes :: UArray Int Int
sampleArrWithZeroes = listArray (0, 4) [0, 10, 12, 0, 30]

{-

>>> sampleRight
>>> replaceZeroes sampleRight (-1)
array (0,4) [(0,0),(1,0),(2,0),(3,0),(4,0)]
array (0,4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]

>>> sampleArrWithZeroes
>>> replaceZeroes sampleArrWithZeroes (-1)
array (0,4) [(0,0),(1,10),(2,12),(3,0),(4,30)]
array (0,4) [(0,-1),(1,10),(2,12),(3,-1),(4,30)]

-}
