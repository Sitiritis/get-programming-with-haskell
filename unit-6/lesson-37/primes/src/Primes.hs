module Primes where


sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime : rest) = nextPrime : sieve noFactors
  where noFactors = filter ((/= 0) . (`mod` nextPrime)) rest

primesLimit :: Int
primesLimit = 10000

primes :: [Int]
primes = sieve [2 .. primesLimit]
-- primes = 1 : sieve [2 .. primesLimit]

-- isPrime :: Int -> Bool
-- isPrime = (`elem` primes)

numIsUnsafe :: Int -> Bool
numIsUnsafe n = n < 2 || n > primesLimit

numIsSafe :: Int -> Bool
numIsSafe = not . numIsUnsafe

isPrime :: Int -> Maybe Bool
isPrime n | numIsUnsafe n = Nothing
          | otherwise = Just $ n `elem` primes

unsafePrimeFactors :: Int -> [Int] -> [Int]
-- unsafePrimeFactors 0 [] = []
unsafePrimeFactors n [] = []
unsafePrimeFactors n (next : restPrimes)
  | n `mod` next == 0 = next : unsafePrimeFactors (n `div` next) (next : restPrimes)
  | otherwise = unsafePrimeFactors n restPrimes

primeFactors :: Int -> Maybe [Int]
primeFactors n | numIsUnsafe n = Nothing
               | otherwise = Just $ unsafePrimeFactors n primesLessThanN
  where primesLessThanN = filter (<= n) primes
