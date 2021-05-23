module Name where


askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"


main :: IO ()
main = askForName >>
       getLine >>=
       return . nameStatement >>=
       putStrLn
