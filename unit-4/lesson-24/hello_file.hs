import System.IO

main :: IO ()
main = do
  -- myFile <- openFile "hello.txt" ReadMode
  -- hClose myFile

  helloFile <- openFile "hello.txt" ReadMode
  let hfGetLine = hGetLine helloFile

  firstLine <- hfGetLine
  putStrLn firstLine

  secondLine <- hfGetLine

  goodbyeFile <- openFile "goodbye.txt" WriteMode
  let gfPutStrLn = hPutStrLn goodbyeFile

  gfPutStrLn secondLine

  hClose helloFile
  hClose goodbyeFile

  putStrLn "done!"
