import Data.List ( sort )

{-|

>>> pure "Hello, World!" :: IO String
"Hello, World!"

Apparently, the string was printed.

>>> (\x y -> (x, y)) <$> [100, 200, 300] <*> [1, 2]
[(100,1),(100,2),(200,1),(200,2),(300,1),(300,2)]

|-}

doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

boxPrize :: [Int]
boxPrize = [500, 20000]

-- Deterministic version
-- totalPrize :: Int
-- totalPrize = (+) doorPrize boxPrize

-- Non-deterministic version
totalPrize :: [Int]
totalPrize = (+) <$> doorPrize <*> boxPrize

{-|

>>> totalPrize
[1500,21000,2500,22000,3500,23000]

|-}

boxMult :: [Int]
boxMult = [10, 50]

totalPrizeMult :: [Int]
totalPrizeMult = (*) <$> doorPrize <*> boxMult

{-|

>>> totalPrizeMult
[10000,50000,20000,100000,30000,150000]

|-}

{-|

>>> (*) <$> [2 .. 4] <*> [2 .. 4]
[4,6,8,6,9,12,8,12,16]

|-}

primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
  where
    twoThroughN = [2 .. n]
    composites = (*) <$> twoThroughN <*> twoThroughN
    isNotComposite = not . (`elem` composites)

-- Users

data User = User {
    name :: String,
    gamerId :: Int,
    score :: Int
  } deriving Show

testNames :: [String]
testNames = [
    "Will Kurt",
    "John Smith",
    "Robert'); DROP TABLE Students;--",
    "Christina NULL",
    "Randall Munroe"
  ]

testIds :: [Int]
testIds = [
    1337,
    0123,
    999999
  ]

testScores :: [Int]
testScores = [
    0,
    100000,
    -99999
  ]

testData :: [User]
testData = User <$> testNames
                <*> testIds
                <*> testScores

-- Q29.1

allFMap :: Applicative f => (a -> b) -> f a -> f b
allFMap func fa = pure func <*> fa

{-|

>>> allFMap (+ 1) [1, 2, 3]
>>> allFMap (+ 1) (Just 5)
>>> allFMap (+ 1) Nothing
[2,3,4]
Just 6
Nothing


|-}

-- Q29.2

example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> (pure (+) <*> pure 2 <*> pure 4) <*> pure 6

{-|

>>> example
>>> exampleMaybe
36
Just 36

|-}

-- Q29.3

beerBrought :: [Int]
beerBrought = [6, 12]

numBeerPerDrunk :: [Int]
numBeerPerDrunk = pure 2

numFriendsTonight :: [Int]
numFriendsTonight = [2, 3]

expectedBeerPerRoommate :: [Int]
expectedBeerPerRoommate = [3, 4]

beersToPurchase :: [Int]
beersToPurchase = sort beersToBuy
  where
    beerDrunk = (* 2) <$> numBeerPerDrunk
    numBeersLeft = (-) <$> beerBrought <*> beerDrunk
    numPeopleTonight = (+ 2) <$> numFriendsTonight
    numBeersForAll = (*) <$> numPeopleTonight <*> expectedBeerPerRoommate
    beersToBuy = (-) <$> numBeersForAll <*> numBeersLeft

{-|

>>> beersToPurchase
[4,7,8,10,12,13,14,18]

|-}
