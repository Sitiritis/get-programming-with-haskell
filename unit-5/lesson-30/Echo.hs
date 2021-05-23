module Echo where


echo :: IO ()
echo = getLine >>= putStrLn

echoVerbose :: IO ()
echoVerbose = putStrLn "Enter a String and it will be echoed!" >>
              getLine >>= putStrLn

main :: IO ()
-- main = echo
main = echoVerbose
