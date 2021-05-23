sumMonadsWithoutDo :: IO ()
sumMonadsWithoutDo = res >>= print
  -- where plusFirst = getLine >>= return . (+) . read
  --       res :: IO Integer
  --       res = read <$> getLine
  where res = getLine >>= (\x -> getLine >>= (\y -> return $ (read x) + (read y)))

sumWithoutDo :: IO ()
sumWithoutDo = (+) <$> (read <$> getLine) <*> (read <$> getLine) >>= print


readPairInt :: IO (Integer, Integer)
readPairInt = x >>= (\a -> y >>= (\b -> return (a, b)))
  where
    x = read <$> getLine
    y = read <$> getLine

maxPairM :: (Monad m, Ord a) => m (a, a) -> m a
maxPairM maa = (\x -> fst x `max` snd x) <$> maa
---- equivalent in this case to this:
-- maxPairM maa = maa >>= (\x -> return $ fst x `max` snd x)
---- equivalent
-- maxPairM maa = do
--   pa <- maa
--   return $ fst pa `max` snd pa
---- but the above is not equivalent to this:
-- maxPairM maa = max <$> (fst <$> maa) <*> (snd <$> maa)

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

helloPersonMain :: IO ()
helloPersonMain = do
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

helloPersonMainDesugared :: IO ()
helloPersonMainDesugared = getLine >>=
  (\name -> 
    (\statement -> putStrLn statement) $ helloPerson name)
-- helloPersonMainDesugared = getLine >>= putStrLn . helloPerson

echo :: IO ()
echo = do
  str <- getLine
  putStrLn str

