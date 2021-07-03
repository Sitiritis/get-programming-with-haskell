import Text.Parsec

getList :: Parsec String u [String]
getList = digits `sepBy` semi
  where
    digits = many1 digit
    semi = char ';'

{-

>>> parseTest getList "1;234;56"
["1","234","56"]

>>> parseTest getList "1;234;56;"
parse error at (line 1, column 10):
unexpected end of input
expecting digit
 
>>> parseTest getList "1;;234;56"
parse error at (line 1, column 3):
unexpected ";"
expecting digit

-}

ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces openBrace closeBrace content = openBrace *> content <* closeBrace

{-

>>> test = ignoreBraces (string "[[") (string "]]") (many1 letter)
>>> parseTest test "[[ABC]]DEF"
"ABC"

-}
