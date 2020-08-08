{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import Text.Read

import Relude.Monoid (maybeToMonoid)

-- toInts :: String -> [Int]
-- toInts = map read . lines

-- main :: IO ()
-- main = do
--   userInput <- getContents
--   let numbers = toInts userInput
--   print (sum numbers)

instance Semigroup Integer where
  (<>) = (+)

instance Monoid Integer where
  mempty = 0

toInts :: T.Text -> [Integer]
toInts contents = nums
  where split = T.lines contents
        maybeNums = map (readMaybe . T.unpack) split :: [Maybe Integer]
        nums = map maybeToMonoid maybeNums

main :: IO ()
main = do
  userInput <- TIO.getContents
  let numbers = toInts userInput
  print (sum numbers)

  return ()
