module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Text.Encoding ( decodeUtf8 )
import Network.HTTP.Simple
import Network.HTTP.Types.Status ( Status(Status, statusCode, statusMessage) )
import Lib

main :: IO ()
main = do
  response <- httpLBS request
  let statusCode = getResponseStatusCode response
  let Status{statusCode=statCode, statusMessage=statMessage} = getResponseStatus response
  case statCode of
    200 -> do
      print "Saving response to file"
      let jsonBody = getResponseBody response
      L.writeFile "data.json" jsonBody
    _ -> print $ "Request failed with error: " <> decodeUtf8 statMessage
  return ()
