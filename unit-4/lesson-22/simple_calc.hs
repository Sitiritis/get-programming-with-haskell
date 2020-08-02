import Data.List.Split
import Safe
import Text.Read

import Control.Applicative (Applicative(liftA2))
import Control.Monad (join)

main :: IO ()
main = do
  input <- getContents
  let inputLines = lines input
      results = map (lineToResult getOperatorForToken) inputLines

  mapM_ (putStrLn . calcResultToString) results

  return ()

calcResultToString :: Maybe Integer -> String
calcResultToString (Just number) = show number
calcResultToString Nothing =
  "Please, enter expression of the form:\n" <>
  "<number> { + | * } <number>\n" <>
  "where <number> is some integer number, e. g. 42"

getOperatorForToken :: String -> Maybe (Integer -> Integer -> Integer)
getOperatorForToken "+" = return (+)
getOperatorForToken "*" = return (*)
getOperatorForToken _   = Nothing

lineToResult :: (String -> Maybe (Integer -> Integer -> Integer)) -> String -> Maybe Integer
lineToResult operatorFor line = do
  let tokens = words line
  firstToken  <- tokens `atMay` 0
  secondToken <- tokens `atMay` 1
  thirdToken  <- tokens `atMay` 2

  leftOperand  <- readMaybe firstToken :: Maybe Integer
  rightOperand <- readMaybe thirdToken :: Maybe Integer

  operator <- operatorFor secondToken

  return $ leftOperand `operator` rightOperand
