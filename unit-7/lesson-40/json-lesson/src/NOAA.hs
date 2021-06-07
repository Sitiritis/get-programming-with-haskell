module NOAA where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics

data NOAAResult = NOAAResult
  { uid :: T.Text
  , mindate :: T.Text
  , maxdate :: T.Text
  , name :: T.Text
  , datacoverage :: Double
  , resultId :: T.Text
  } deriving Show

instance FromJSON NOAAResult where
  parseJSON = withObject "" $ \obj ->
    NOAAResult <$> obj .: "uid"
               <*> obj .: "mindate"
               <*> obj .: "maxdate"
               <*> obj .: "name"
               <*> obj .: "datacoverage"
               <*> obj .: "id"

instance ToJSON NOAAResult where
  toJSON noaaResult =
    object [ "uid"          .= uid noaaResult
           , "mindate"      .= mindate noaaResult
           , "maxdate"      .= maxdate noaaResult
           , "name"         .= name noaaResult
           , "datacoverage" .= datacoverage noaaResult
           , "id"           .= resultId noaaResult
           ]

data Resultset = Resultset
  { offset :: Int
  , count :: Int
  , limit :: Int
  } deriving (Show, Generic)

instance FromJSON Resultset
instance ToJSON Resultset

newtype Metadata = Metadata { resultset :: Resultset } deriving (Show, Generic)

instance FromJSON Metadata
instance ToJSON Metadata

data NOAAResponse = NOAAResponse
  { metadata :: Metadata
  , results :: [NOAAResult]
  } deriving (Show, Generic)

instance FromJSON NOAAResponse
instance ToJSON NOAAResponse
