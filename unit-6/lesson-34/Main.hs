{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Palindrome ( isPalindrome )
import qualified Palindrome
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

head :: (Monoid a) => [a] -> a
head (x:_) = x
head [] = mempty

example :: [[Int]]
example = []

{-

>>> head example
Ambiguous occurrence ‘head’
It could refer to
   either ‘Prelude.head’,
          imported from ‘Prelude’ at ~/get-programming-with-haskell/unit-6/lesson-34/Main.hs:1:8-11
          (and originally defined in ‘GHC.List’)
       or ‘Main.head’,
          defined at ~/get-programming-with-haskell/unit-6/lesson-34/Main.hs:4:1

>>> Main.head example
[]

>>> Prelude.head example
Prelude.head: empty list

-}

isPalindrome :: T.Text -> Bool
isPalindrome text = text == T.reverse text


main :: IO ()
main = do
  TIO.putStrLn "Enter a word and I'll let you know if it's a palindrome!"
  text <- TIO.getLine
  let response = if Palindrome.isPalindrome text
                 then "it is!"
                 else "it's not!"
  print response
