import Pizza

main :: IO ()
main = do
  putStrLn "What is the size of the 1-st pizza?"
  size1 <- getLine
  putStrLn "What is the cost of the 1-st pizza?"
  cost1 <- getLine

  putStrLn "What is the size of the 2-nd pizza?"
  size2 <- getLine
  putStrLn "What is the cost of the 2-nd pizza?"
  cost2 <- getLine

  let pizza1 = (read size1, read cost1)
  let pizza2 = (read size2, read cost2)

  let betterPizza = comparePizzas pizza1 pizza2

  putStrLn $ cheeperPizzaMessage betterPizza
