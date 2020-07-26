import System.Random

helloPerson :: String -> String
helloPerson name = mconcat ["Hello, ", name, "!"]

main :: IO ()
main = do
  putStrLn "Hello! What's your name?"
  name <- getLine
  let statement = helloPerson name
  putStrLn statement
