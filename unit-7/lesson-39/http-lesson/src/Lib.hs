module Lib where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Config ( myToken )
import NoaaAPI ( noaaHost, apiPath )

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString
             -> BC.ByteString -> Int -> Request
buildRequest token host method path port =
  setRequestMethod method $
  setRequestHost host $
  setRequestHeader "token" [token] $
  setRequestPath path $
  setRequestSecure True $
  setRequestPort port $
  defaultRequest

buildRequestSSL :: BC.ByteString -> BC.ByteString -> BC.ByteString
             -> BC.ByteString -> Request
buildRequestSSL token host method path =
  buildRequest token host method path 443

buildRequestNOSSL :: BC.ByteString -> BC.ByteString -> BC.ByteString
                  -> BC.ByteString -> Request
buildRequestNOSSL token host method path =
    buildRequest token host method path 80

request :: Request
request = buildRequestSSL myToken noaaHost "GET" apiPath
