{-# LANGUAGE OverloadedStrings #-}

module CharByteDiff where

import TextShow ( TextShow(showt) )
import System.Environment ( getArgs )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as B

import Safe

-- Q25.1 Write a program that reads in a text file and outputs the difference
-- between the number of characters in the file and the number of bytes in the file.

main :: IO ()
main = do
  args <- getArgs
  case headMay args of
    Just filePath -> do
      fileBytes <- B.readFile filePath
      let numBytes = B.length fileBytes
      TIO.putStrLn $ "# of bytes: " <> showt numBytes

      textContents <- TIO.readFile filePath
      let numChars = T.length textContents
      TIO.putStrLn $ "# of characters: " <> showt numChars

      TIO.putStrLn $ "bytes - chars: " <> showt (numBytes - numChars)

    Nothing -> TIO.putStrLn "The first argument must be path to file to compare number of bytes against characters"
