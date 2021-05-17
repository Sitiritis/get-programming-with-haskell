{-# LANGUAGE OverloadedStrings #-}

module Min3 where

import qualified Data.Map as Map


minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree x y z = min x $ min y z

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt


{-|

>>> maybeTen = Just 10
>>> maybeThree = Just 3
>>> maybeSix = Just 6
>>> minOfThree <$> maybeTen <*> maybeThree <*> maybeSix
Just 3

|-}


main :: IO ()
main = do
  putStrLn "Enter 3 numbers separated by new lines"
  minInt <- minOfInts
  putStrLn $ show minInt ++ " is the smallest"
