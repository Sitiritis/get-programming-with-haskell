import Text.Read

main :: IO ()
main = do
  userInput <- getContents
  let maybeNumbers = linesToInts userInput
  putStrLn $ case maybeNumbers of
    Just numbers -> mconcat [
        "Sum: ", sumInput numbers, "\n"
      , "Sum of squares: ", sumSquaredInput numbers
      ]
    Nothing -> "Not a number has been entered"

  return ()
-- main = reverser

reverser :: IO ()
reverser = do
  input <- getContents
  let reversed = reverse input
  putStrLn reversed

linesToInts :: String -> Maybe [Integer]
-- linesToInts = map read . lines
linesToInts = traverse readMaybe . lines

inputToResult :: (Show a, Num a) => (Integer -> a) -> [Integer] -> String
inputToResult f numbers = show $ sum (map f numbers)

sumInput :: [Integer] -> String
sumInput = inputToResult id

sumSquaredInput :: [Integer] -> String
sumSquaredInput = inputToResult (^ 2)
