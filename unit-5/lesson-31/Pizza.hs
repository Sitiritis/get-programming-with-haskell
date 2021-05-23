import qualified Data.Map as Map

type Pizza = (Double, Double)

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2)^2

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> (Bool, Pizza)
comparePizzas p1 p2
  | costP1 > costP2 = (True, p1)
  | otherwise = (False, p2)
  where costP1 = costPerInch p1
        costP2 = costPerInch p2

cheeperPizzaMessage :: (Bool, Pizza) -> String
cheeperPizzaMessage (pizzaIdx, p@(size, cost)) =
  "The " ++ pizzaNumText ++ " pizza" ++
  " is cheaper at " ++
  show costSqInch ++
  " per square inch"
  where
    pizzaNumText = case pizzaIdx of
      True  -> "1-st"
      False -> "2-nd"
    costSqInch = costPerInch p

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

-- Q31.1

mainDesugared :: IO ()
mainDesugared =
  putStrLn "What is the size of the 1-st pizza?" >>
  getLine >>= (\size1 ->
    putStrLn "What is the cost of the 1-st pizza?" >>
    getLine >>= (\cost1 ->
      putStrLn "What is the size of the 2-nd pizza?" >>
      getLine >>= (\size2 ->
        putStrLn "What is the cost of the 2-nd pizza?" >>
        getLine >>= (\cost2 ->
          (\pizza1 -> 
            (\pizza2 -> 
              (\betterPizza -> 
                putStrLn $ cheeperPizzaMessage betterPizza
                ) (comparePizzas pizza1 pizza2)
              ) (read size2, read cost2)
            ) (read size1, read cost1)
          )
        )
      )
    )

-- Q31.2
sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1, 20.0), (2, 15.0)]

costData :: Map.Map Int Double
costData = Map.fromList [(1, 18.0), (2, 16.0)]

maybeMainPizza :: Maybe String
maybeMainPizza = do
  size1 <- Map.lookup 1 sizeData
  cost1 <- Map.lookup 1 costData
  size2 <- Map.lookup 2 sizeData
  cost2 <- Map.lookup 2 costData

  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)

  let betterPizza = comparePizzas pizza1 pizza2

  return $ cheeperPizzaMessage betterPizza

maybeMainPizzaDesugared :: Maybe String
maybeMainPizzaDesugared =
  Map.lookup 1 sizeData >>= (\size1 ->
    Map.lookup 1 costData >>= (\cost1 ->
      Map.lookup 2 sizeData >>= (\size2 ->
        Map.lookup 2 costData >>= (\cost2 ->
            (\pizza1 -> 
              (\pizza2 ->
                (\betterPizza ->
                  return $ cheeperPizzaMessage betterPizza
                  ) (comparePizzas pizza1 pizza2)
                ) (size2, cost2)
              ) (size1, cost1)
          )
        )
      )
    )

-- Q31.3

mainPizzaRefined :: (Monad m) => m Pizza -> m Pizza -> m String
mainPizzaRefined mPizza1 mPizza2 = do
  pizza1 <- mPizza1
  pizza2 <- mPizza2
  let betterPizza = comparePizzas pizza1 pizza2
  return $ cheeperPizzaMessage betterPizza
