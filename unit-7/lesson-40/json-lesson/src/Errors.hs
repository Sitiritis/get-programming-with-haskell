module Errors where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics

sampleError :: BC.ByteString
sampleError = "{\"message\": \"oops!\", \"error\": 123}"

data ErrorMessage = ErrorMessage
  { message :: T.Text
  , errorCode :: Int
  } deriving Show

instance FromJSON ErrorMessage where
  parseJSON = withObject "" $ \obj ->
    ErrorMessage <$> obj .: "message"
                 <*> obj .: "error"

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage message errorCode) =
    object [ "message" .= message
           , "error" .= errorCode
           ]

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "Everything is Okay" 0
