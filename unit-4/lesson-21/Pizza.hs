module Pizza where

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2)^2

type Pizza = (Double, Double)

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
