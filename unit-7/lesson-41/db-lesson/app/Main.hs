module Main where

import Control.Applicative
import Data.List ( intercalate )
import Domain.Logic.Tool
import Domain.Logic.User

promptAndAddUser :: IO ()
promptAndAddUser = do
  print "Enter new user name"
  userName <- getLine
  addUser userName

promptAndAddTool :: IO ()
promptAndAddTool = do
  print "Enter new tool name"
  name <- getLine
  print "Enter new tool description"
  description <- getLine
  addTool name description

promptAndCheckout :: IO ()
promptAndCheckout = do
  print "Enter the id of the user"
  userId <- read <$> getLine
  print "Enter the id of the tool"
  toolId <- read <$> getLine
  checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
  print "Enter the id of the tool"
  toolId <- read <$> getLine
  checkinAndUpdate toolId

performCommand :: String -> IO ()
performCommand command
  | command == usersCmd           = printUsers        >> main
  | command == toolsCmd           = printTools        >> main
  | command == adduserCmd         = promptAndAddUser  >> main
  | command == addtoolCmd         = promptAndAddTool  >> main
  | command == checkoutCmd        = promptAndCheckout >> main
  | command == checkinCmd         = promptAndCheckin  >> main
  | command == availableToolsCmd  = printAvailable    >> main
  | command == checkedOutToolsCmd = printCheckedout   >> main
  | command == quitCmd            = print "Bye!"
  | otherwise =
      print ("Only the following commands are supported: " <>
             intercalate ", " availableCommands)
      >> main
  where
    usersCmd = "users"
    toolsCmd = "tools"
    adduserCmd = "adduser"
    addtoolCmd = "addtool"
    checkoutCmd = "checkout"
    checkinCmd = "checkin"
    availableToolsCmd = "in"
    checkedOutToolsCmd = "out"
    quitCmd = "quit"
    availableCommands = [ usersCmd
                        , toolsCmd
                        , adduserCmd
                        , addtoolCmd
                        , checkoutCmd
                        , checkinCmd
                        , availableToolsCmd
                        , checkedOutToolsCmd
                        , quitCmd
                        ]

main :: IO ()
main = do
  print "Enter a command"
  command <- getLine
  performCommand command
