module Utils.DbUtils where

import Database.SQLite.Simple
import Config

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
  conn <- open dbName
  action conn
  close conn

withConnToolsDb :: (Connection -> IO ()) -> IO ()
withConnToolsDb = withConn toolsDbName
