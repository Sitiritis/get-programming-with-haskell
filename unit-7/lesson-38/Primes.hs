module Primes where

primes :: [Int]
primes = [2, 3, 5, 7]

maxN :: Int
maxN = 10

isPrimeMaybe :: Int -> Maybe Bool
isPrimeMaybe n
  | n < 2 = Nothing
  | n > maxN = Nothing
  | otherwise = Just (n `elem` primes)

isPrimeEitherStringBool :: Int -> Either String Bool
isPrimeEitherStringBool n
  | n < 2 = Left "Numbers less than 2 are not candidates for primes"
  | n > maxN = Left "Value exceeds limits of prime checker"
  | otherwise = Right (n `elem` primes)

{-

>>> isPrime 5
Right True

>>> isPrime 6
Right False

>>> isPrime 100
Left "Value exceeds limits of prime checker"

>>> isPrime (-29)
Left "Numbers less than 2 are not candidates for primes"

-}

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
  show TooLarge = "Value exceed max bound"
  show InvalidValue = "Value is not a valid candidate for prime checking"

isPrime :: Int -> Either PrimeError Bool
isPrime n
  | n < 2 = Left InvalidValue
  | n > maxN = Left TooLarge
  | otherwise = Right (n `elem` primes)

{-

>>> isPrime 99
Left Value exceed max bound

>>> isPrime 0
Left Value is not a valid candidate for prime checking

-}

dispalyResult :: Either PrimeError Bool -> String
dispalyResult (Right True) = "It's prime"
dispalyResult (Right False) = "It's composite"
dispalyResult (Left primeError) = show primeError


main :: IO ()
main = do
  print "Enter a number to test for primality:"
  n <- read <$> getLine
  let result = isPrime n
  print $ dispalyResult result
