import Control.Applicative hiding (many)
import Data.Char (digitToInt)
import GHC.Unicode (isDigit, isLower)

-- type Parser a = String -> [(a, String)]
-- is equivalent to standard ReadS type

-- Define newtype, so that new typeclass instances can be defined for the type
newtype Parser a = Parser {apply :: String -> [(a, String)]}

parse :: Parser a -> String -> a
parse p = fst . head . apply p

anyChar :: Parser Char
anyChar = Parser f
 where
  f "" = []
  f (c : cs) = [(c, cs)]

instance Functor Parser where
  fmap f (Parser p) = Parser appliedP
   where
    appliedP s = [(f a, s') | (a, s') <- p s]

instance Applicative Parser where
  pure a = Parser $ \s -> [(a, s)]
  pf <*> pv = Parser parser
   where
    parser s = [(f a, s'') | (f, s') <- apply pf s, (a, s'') <- apply pv s']

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser parser
 where
  parser "" = []
  parser (c : cs)
    | p c = [(c, cs)]
    | otherwise = []

lower :: Parser Char
lower = satisfy isLower

char :: Char -> Parser Char
char c = satisfy (== c)

-- Unsafe
digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

multiplication :: Parser Int
multiplication = (*) <$> digit <* char '*' <*> digit

instance Alternative Parser where
  empty = Parser $ const []
  (Parser pl) <|> (Parser pr) = Parser parser
   where
    parser s =
      let lr = pl s
       in if null lr
            then pr s
            else lr

lowers :: Parser String
lowers = pure (:) <*> lower <*> lowers <|> pure ""

many :: Parser a -> Parser [a]
many p = (:) <$> p <*> many p <|> pure []
