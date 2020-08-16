{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Maybe

import System.Environment

import Safe

main :: IO ()
main = do
  args <- getArgs

  let maybeCapitalizeEffect = do
      fileName <- args `atMay` 0
      return $ do
          contents <- TIO.readFile fileName
          TIO.writeFile ("capitalized_" <> fileName) (T.toUpper contents)

  fromMaybe (TIO.putStrLn "Please, provide file name as a first command line argument to the program.") maybeCapitalizeEffect
