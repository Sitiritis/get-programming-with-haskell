module Lib where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import Control.Monad
import GHC.Generics
import NOAA

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = print "Error while parsing json"
printResults (Just results) = do
  forM_ results (print . name)
