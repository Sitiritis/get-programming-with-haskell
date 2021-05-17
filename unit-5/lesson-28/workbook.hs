addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just x) (Just y) = Just (x + y)
addMaybe _ _ = Nothing

-- Not able to define the function in terms of Functor (as expected)
-- map2 :: Functor f => (a -> b -> c) -> f a -> f b -> f c
-- map2 func fa fb = _
--   where
--     partial = func <$> fa
--     x = (\bToc -> bToc <$> fb) <$> partial

val1 = Just 10
val2 = Just 5

res1 = (*) <$> val1 <*> val2
res2 = div <$> val1 <*> val2
res3 = mod <$> val1 <*> val2


data User = User {
    name :: String,
    gamerId :: Int,
    score :: Int
  } deriving Show

{-|

>>> User { name = "Sue", gamerId = 1337, score = 9001 }
User {name = "Sue", gamerId = 1337, score = 9001}

>>> User "Sue" 1337 9001
User {name = "Sue", gamerId = 1337, score = 9001}

|-}

serverUsername :: Maybe String
serverUsername = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

{-|

>>> User <$> serverUsername <*> serverGamerId <*> serverScore
Just (User {name = "Sue", gamerId = 1337, score = 9001})

|-}

readInt :: IO Int
readInt = read <$> getLine

main :: IO ()
main = do
  putStrLn "Enter username, gamerId and score separated by new lines"
  user <- User <$> getLine <*> readInt <*> readInt
  print user

{-|

>>> User <$> Nothing <*> serverGamerId <*> serverScore
Nothing

|-}
