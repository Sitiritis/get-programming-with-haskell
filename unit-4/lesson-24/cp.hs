{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Maybe

import System.Environment

import Safe

main :: IO ()
main = do
  args <- getArgs

  let maybeSourceFile = args `atMay` 0
  let maybeTargetFile = args `atMay` 1

  let maybeCopyEffect = do
      sourceFile <- maybeSourceFile
      targetFile <- maybeTargetFile
      return $ do
          sourceContents <- TIO.readFile sourceFile
          TIO.writeFile targetFile sourceContents

  fromMaybe (TIO.putStrLn $ mconcat [
      "The command must be called as:\n",
      "cp SOURCE DESTINATION"
    ]) maybeCopyEffect
