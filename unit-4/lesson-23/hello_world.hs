{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- helloPerson :: String -> String
-- helloPerson name = "Hello" ++ " " ++ name ++ "!"

-- main :: IO ()
-- main = do
--   putStrLn "Hello! What's your name?"
--   name <- getLine
--   let statement = helloPerson name
--   putStrLn statement

helloPerson :: T.Text -> T.Text
helloPerson name = mconcat ["Hello, ", name, "!"]

main :: IO ()
main = do
  TIO.putStrLn "Hello! What's your name?"
  name <- TIO.getLine
  let statement = helloPerson name
  TIO.putStrLn statement

  return ()
