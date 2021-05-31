import Test.QuickCheck
import Primes
import Data.Maybe ( isJust, isNothing, maybe, fromMaybe )


prop_validPrimesOnly val | val < 2 || val > primesLimit = isNothing result
                         | otherwise = isJust result
  where result = isPrime val

prop_primesArePrime val | result == Just True = null divisors
                        | otherwise = True
  where result = isPrime val
        divisors = filter ((== 0) . (val `mod`)) [2 .. val - 1]

prop_nonPrimesAreComposite val | result == Just False = not $ null divisors
                               | otherwise = True
  where result = isPrime val
        divisors = filter ((== 0) . (val `mod`)) [2 .. val - 1]

-- prop_factorsMakeOriginal val | isNothing result = True
--                              | otherwise = maybe False ((== val) . product) <$> result
--   where result = primeFactors val

prop_factorsMakeOriginal val = maybe True ((== val) . product) result
  where result = primeFactors val

prop_allFactorsPrime val = fromMaybe True maybeAllFactorsPrime
  where result = primeFactors val
        maybeFactorsPrime = result >>= (sequence . (isPrime <$>))
        maybeAllFactorsPrime = and <$> maybeFactorsPrime


main :: IO ()
main = do
  quickCheck prop_validPrimesOnly
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_primesArePrime
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_nonPrimesAreComposite
  quickCheck prop_factorsMakeOriginal
  quickCheck prop_allFactorsPrime
