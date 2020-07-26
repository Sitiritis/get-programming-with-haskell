import System.Random
import Data.Map as Map

import Pizza

helloPerson :: String -> String
helloPerson name = mconcat ["Hello, ", name, "!"]

main :: IO ()
main = do
  putStrLn "Hello! What's your name?"
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

userName :: Map.Map String String
userName = Map.fromList [("name", "Tymur")]

maybeMainHellower :: Maybe String
maybeMainHellower = do
  name <- Map.lookup "name" userName
  return $ helloPerson name

-- Pizza example

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
