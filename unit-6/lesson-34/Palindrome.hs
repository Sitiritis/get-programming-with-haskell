module Palindrome ( isPalindrome
                  , preprocess
                  ) where

import Data.Char ( toLower, isSpace, isPunctuation )
import qualified Data.Text as T

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace = T.filter $ not . isSpace

stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.filter $ not . isPunctuation

toLowerCase :: T.Text -> T.Text
toLowerCase = T.map toLower

preprocess :: T.Text -> T.Text
preprocess = toLowerCase . stripWhiteSpace . stripPunctuation


isPalindrome :: T.Text -> Bool
isPalindrome text = processedText == T.reverse processedText
  where processedText = preprocess text
