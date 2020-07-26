fibAux :: Integer -> Integer -> Integer -> Integer
fibAux _ b 0 = b
fibAux a b n = fibAux b (a + b) (n - 1)

fib :: Integer -> Integer
-- fib 0 = 1
-- fib 1 = 1
-- fib n = fib (n - 1) + fib (n - 2)
fib n = fibAux 0 1 n


main :: IO ()
main = do
  putStrLn "Fibonacci number"
  nString <- getLine
  let n = read nString
  putStrLn $ show (fib n)