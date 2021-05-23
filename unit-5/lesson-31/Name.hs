module Name where


askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >>
            getLine >>=
            return . nameStatement >>=
            putStrLn

helloNameDo :: IO ()
helloNameDo = do
  askForName
  name <- getLine
  print $ nameStatement name


main :: IO ()
-- main = helloName
main = helloNameDo
