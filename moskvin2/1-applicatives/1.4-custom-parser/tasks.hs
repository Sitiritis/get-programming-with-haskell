import Control.Applicative hiding (many)
import qualified Data.Bifunctor
import Data.Char (digitToInt, isDigit)

newtype Prs a = Prs {runPrs :: String -> Maybe (a, String)}

instance Functor Prs where
  fmap f (Prs p) = Prs prs
   where
    prs s = Data.Bifunctor.first f <$> p s

anyChr :: Prs Char
anyChr = Prs f
 where
  f "" = Nothing
  f (c : cs) = Just (c, cs)

{-

>>> runPrs anyChr "ABC"
>>> runPrs anyChr ""
>>> runPrs (digitToInt <$> anyChr) "BCD"
Just ('A',"BC")
Nothing
Just (11,"CD")

-}

instance Applicative Prs where
  pure a = Prs $ \s -> Just (a, s)
  (Prs pf) <*> (Prs pv) = Prs parser
   where
    parser s = do
      (f, s') <- pf s
      (a, s'') <- pv s'
      return (f a, s'')

{-

>>> runPrs ((,,) <$> anyChr <*> anyChr <*> anyChr) "ABCDE"
>>> runPrs (anyChr *> anyChr) "ABCDE"
Just (('A','B','C'),"DE")
Just ('B',"CDE")

-}

instance Alternative Prs where
  empty = Prs $ const Nothing
  (Prs pl) <|> (Prs pr) = Prs parser
   where
    parser s =
      let lr = pl s
       in if null lr
            then pr s
            else lr

satisfy :: (Char -> Bool) -> Prs Char
satisfy p = Prs parser
 where
  parser "" = Nothing
  parser (c : cs)
    | p c = Just (c, cs)
    | otherwise = Nothing

char :: Char -> Prs Char
char c = satisfy (== c)

{-

>>> runPrs (char 'A' <|> char 'B') "ABC"
>>> runPrs (char 'A' <|> char 'B') "BCD"
>>> runPrs (char 'A' <|> char 'B') "CDE"
Just ('A',"BC")
Just ('B',"CD")
Nothing

-}

many1 :: Prs a -> Prs [a]
many1 p = (:) <$> p <*> (many1 p <|> pure [])

{-

>>> runPrs (many1 $ char 'A') "AAABCDE"
>>> runPrs (many1 $ char 'A') "BCDE"
Just ("AAA","BCDE")
Nothing

-}

digit :: Prs Char
digit = satisfy isDigit

digits :: Prs String
digits = many1 digit

nat :: Prs Int
nat = read <$> digits

mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat

{-

>>> runPrs mult "14*3"
>>> runPrs mult "64*32"
>>> runPrs mult "77*0"
>>> runPrs mult "2*77AAA"
Just (42,"")
Just (2048,"")
Just (0,"")
Just (154,"AAA")

-}

newtype PrsE a = PrsE {runPrsE :: String -> Either String (a, String)}

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE p = PrsE parser
 where
  parser "" = Left "unexpected end of input"
  parser (c : cs)
    | p c = Right (c, cs)
    | otherwise = Left $ "unexpected " ++ [c]

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

{-

>>> runPrsE (charE 'A') "ABC"
>>> runPrsE (charE 'A') "BCD"
>>> runPrsE (charE 'A') ""
Right ('A',"BC")
Left "unexpected B"
Left "unexpected end of input"

-}

instance Functor PrsE where
  fmap f (PrsE p) = PrsE parser
   where
    parser s = Data.Bifunctor.first f <$> p s

instance Applicative PrsE where
  pure a = PrsE $ \s -> Right (a, s)
  (PrsE pf) <*> (PrsE pv) = PrsE parser
   where
    parser s = do
      (f, s') <- pf s
      (a, s'') <- pv s'
      return (f a, s'')

{-

>>> let anyE = satisfyE (const True)
>>> runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "ABCDE"
>>> runPrsE ((,) <$> anyE <* charE 'C' <*> anyE) "ABCDE"
>>> runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "AB"
Right (('A','C'),"DE")
Left "unexpected B"
Left "unexpected end of input"

-}
