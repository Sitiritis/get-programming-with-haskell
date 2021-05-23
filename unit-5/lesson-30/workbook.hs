{-# LANGUAGE KindSignatures #-}

readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble n = print $ n * 2

readAndPrintDouble :: IO ()
readAndPrintDouble = readInt >>= printDouble

{-|

>>> :t return . (+ 2) :: Num a => a -> IO a
return . (+ 2) :: Num a => a -> IO a :: forall a. Num a => a -> IO a

|-}

allFmapM :: Monad (m :: * -> *) => (a -> b) -> m a -> m b
allFmapM func ma = ma >>= return . func

{-|

>>> allFmapM (+ 1) [1, 2, 3]
[2,3,4]

|-}

allApp :: Monad (m :: * -> *) => m (a -> b) -> m a -> m b
-- allApp mab ma = mab >>= (\f -> ma >>= return . f)
allApp mab ma = mab >>= (<$> ma)

{-|

>>> allApp (return (+ 1)) (Just 1)
Just 2

>>> ((+) <$> Just 2) `allApp` Just 3
Just 5

|-}

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing f = Nothing
bind (Just a) f = f a
