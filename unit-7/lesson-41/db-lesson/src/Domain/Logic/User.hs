module Domain.Logic.User where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Domain.Data.User
import Utils.DbUtils

instance FromRow User where
  fromRow = User <$> field
                 <*> field

-- addUser :: String -> IO ()
-- addUser userName = do
--   conn <- open "tools.db"
--   execute conn "insert into users (username) values (?)" (Only userName)
--   print $ "User \"" ++ userName ++ "\" successfully added!"
--   close conn

addUser :: String -> IO ()
addUser userName = withConnToolsDb $ \conn -> do
  execute conn "insert into users (username) values (?);"
    (Only userName)
  print $ "User \"" ++ userName ++ "\" successfully added!"

printUsers :: IO ()
printUsers = withConnToolsDb $ \conn -> do
  users <- query_ conn "select * from users;" :: IO [User]
  mapM_ print users
