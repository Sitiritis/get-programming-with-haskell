import Text.Read (readMaybe)
import qualified Data.Map as Map

main :: IO ()
main = do
  input <- getContents
  let inputLines = lines input

  mapM_ putStrLn (inputProcessor inputLines)

  return ()

inputProcessor :: [String] -> [String]
inputProcessor ("n" : _) = []
inputProcessor (line : lines) = inputToResult line : inputProcessor lines

inputToResult :: String -> String
inputToResult input = maybeIntToResult $ readMaybe input

maybeIntToResult :: Maybe Int -> String
maybeIntToResult mi = case maybeQuote of
    Just quote -> quote <>
      "\nAnother quote? (Type number (1-5) to print a quote, n to quit)"
    Nothing -> "Please, enter a number from 1 to 5, n to quit"
  where maybeQuote = do
          i <- mi
          Map.lookup i numbersQuotes

numbersQuotes :: Map.Map Int String
numbersQuotes = Map.fromList [
    (1, "OH MY GOD!!!")
  , (2, "Yare yare daze...")
  , (3, "Monad is just a monoid in a category of endofunctors.")
  , (4, mconcat [
          "Да ты пойми, делаешь пандорический захват, "
        , "лифтишь в монаду, потом строишь рекурсивную схему "
        , "(здесь подойдёт зигохистоморфный препроморфизм) "
        , "как монадический трансформер из категории эндофункторов "
        , "и метациклически вычисляешь результат. Любой второкурсник справится."
        ])
  , (5, "Computer is like air conditioner. It becomes useless when you open windows.")
  ]
