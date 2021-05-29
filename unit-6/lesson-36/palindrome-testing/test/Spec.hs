import Lib
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Char ( isPunctuation, isSpace, toLower, toUpper )
import qualified Data.Text as T


assert :: Bool -> String -> String -> IO ()
assert test passStatemet failStatement
  | test = putStrLn passStatemet
  | otherwise = putStrLn failStatement

prop_punctuationInvariant text = preprocess text ==
                                 preprocess noPuncText
  where noPuncText = T.filter (not . isPunctuation) text

prop_reverseInvariant text = isPalindrome text == isPalindrome reversedText
  where reversedText = T.reverse text

prop_whitespaceInvariant text = preprocess text ==
                                preprocess noWhiteSpaceText
  where noWhiteSpaceText = T.filter (not . isSpace) text

prop_lowercaseInvariant text = preprocess text ==
                               preprocess lowercaseText
  where lowercaseText = T.map toLower text

prop_uppercaseInvariant text = preprocess text ==
                               preprocess uppercaseText
  where uppercaseText = T.map toUpper text

prop_capitalizationInvariant text = prop_lowercaseInvariant text &&
                                    prop_uppercaseInvariant text

main :: IO ()
-- main = do
--   putStrLn "Running tests..."
--   assert (isPalindrome "racecar") "passed 'racecar'" "FAIL: 'racecar'"
--   assert (isPalindrome "racecar!") "passed 'racecar'" "FAIL: 'racecar!'"
--   assert ((not . isPalindrome) "cat") "passed 'cat'" "FAIL: 'cat'"
--   assert (isPalindrome "racecar.") "passed 'racecar.'" "FAIL: 'racecar.'"
--   assert (isPalindrome ":racecar:") "passed ':racecar:'" "FAIL: ':racecar:'"
--   putStrLn "done!"
main = do
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
  quickCheck prop_reverseInvariant
  quickCheck prop_whitespaceInvariant
  quickCheck prop_capitalizationInvariant
  putStrLn "done!"
