module Book where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics

data Book = Book
  { title :: T.Text
  , author :: T.Text
  , year :: Int
  } deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book
  { author="Will Kurt"
  , title="Learn Haskell"
  , year = 2017
  }

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

rawJSON :: BC.ByteString
rawJSON = "{\"author\": \"Emil Ciroan\", \"title\": \"A Short Hustory of Decay\", \"year\": 1949}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

wrongJSON :: BC.ByteString
wrongJSON = "{\"writer\": \"Emil Ciroan\", \"title\": \"A Short Hustory of Decay\", \"year\": 1949}"

bookFromWrongJSON :: Maybe Book
bookFromWrongJSON = decode wrongJSON
