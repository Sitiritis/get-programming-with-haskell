module Name where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics

data Name = Name
  { firstName :: T.Text
  , lastName :: T.Text
  } deriving (Show, Generic)

instance FromJSON Name
instance ToJSON Name

data NameNotGeneric = NameNotGeneric
  { firstNameNotGeneric :: T.Text
  , lastNameNotGeneric :: T.Text
  } deriving (Show)

instance FromJSON NameNotGeneric where
  parseJSON = withObject "" $ \obj -> do
    NameNotGeneric <$> obj .: "firstName"
                   <*> obj .: "lastName"

instance ToJSON NameNotGeneric where
  toJSON (NameNotGeneric firstName lastName) =
    object [ "firstName" .= firstName
           , "lastName" .= lastName
           ]

