module Domain.Logic.Tool where

import Data.Maybe ( listToMaybe )
import Data.Time
import Data.Time.Clock.System
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Domain.Data.Tool
import Domain.Data.User
import Utils.DbUtils

instance FromRow Tool where
  fromRow = Tool <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

printTools :: IO ()
printTools = printToolQuery "select * from tools;"

printAvailable :: IO ()
printAvailable = printToolQuery $ mconcat
  [ "select * from tools "
  , "where id not in "
  , "(select tool_id from checkedout);"
  ]

printCheckedout :: IO ()
printCheckedout = printToolQuery $ mconcat
  [ "select * from tools "
  , "where id in "
  , "(select tool_id from checkedout);"
  ]

addTool :: String -> String -> IO ()
addTool name description = withConnToolsDb $ \conn -> do
  execute conn
    "insert into tools (name, description, lastReturned, timesBorrowed) values (?, ?, ?, ?);"
    ((name, description, systemEpochDay, 0) :: (String, String, Day, Int))
  print $ "Tool \"" ++ name ++ "\" successfully added!"

checkout :: UserId -> ToolId -> IO ()
checkout userId toolId = withConnToolsDb $ \conn -> do
  execute conn "insert into checkedout (user_id, tool_id) values (?, ?);"
    (userId, toolId)

printToolQuery :: Query -> IO ()
printToolQuery q = withConnToolsDb $ \conn -> do
  tools <- query_ conn q :: IO [Tool]
  mapM_ print tools

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  tools <- query conn
    "select * from tools where id = (?)"
    (Only toolId) :: IO [Tool]
  return $ listToMaybe tools

updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
  { lastReturned = date
  , timesBorrowed = 1 + timesBorrowed tool
  }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "the tool not found"
updateOrWarn (Just tool) = withConnToolsDb $ \conn -> do
  let q = mconcat [ "update tools set "
                  , "lastReturned = ?, "
                  , "timesBorrowed = ? "
                  , "where id = ?;" ]
  let qParams = ( lastReturned tool
                , timesBorrowed tool
                , toolId tool )
  execute conn q qParams
  print $ mconcat
    [ show $ toolId tool 
    , " (", name tool , ") "
    , " tool updated" ]

updateToolTable :: ToolId -> IO ()
updateToolTable toolId = withConnToolsDb $ \conn -> do
  maybeTool <- selectTool conn toolId
  currentDay <- utctDay <$> getCurrentTime
  let maybeUpdatedTool = updateTool <$> maybeTool
                                    <*> pure currentDay
  updateOrWarn maybeUpdatedTool

checkin :: ToolId -> IO ()
checkin toolId = withConnToolsDb $ \conn -> do
  execute conn
    "delete from checkedout where tool_id = (?);"
    (Only toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
  checkin toolId
  updateToolTable toolId
