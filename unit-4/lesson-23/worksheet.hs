{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Semigroup

main :: IO ()
main = do
  print aWord
  return ()

firstWord :: String
firstWord = "optimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

myNum1 :: Int
myNum1 = 3

myNum2 :: Integer
myNum2 = 3

myNum3 :: Double
myNum3 = 3

aWord :: T.Text
aWord = "Cheese"

sampleInput :: T.Text
sampleInput = "this\nis\ninput"

someText :: T.Text
someText = "Some\ntext for\nyou"

breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"


myLines :: T.Text -> [T.Text]
myLines = T.splitOn "\n"

myUnlines :: [T.Text] -> T.Text
myUnlines = T.intercalate "\n"
