module Lib
    ( isPalindrome
    , preprocess
    ) where

import qualified Data.Text as T
import Data.Char ( isPunctuation, isSpace, toLower )


stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace = T.filter $ not . isSpace

stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.filter $ not . isPunctuation

preprocess :: T.Text -> T.Text
preprocess = T.toLower . stripPunctuation . stripWhiteSpace

isPalindrome :: T.Text -> Bool
-- isPalindrome text = text == reverse text
isPalindrome text = cleanText == T.reverse cleanText
  -- where cleanText = filter (/= '!') text
  where cleanText = preprocess text
