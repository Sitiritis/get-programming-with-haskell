import System.Environment
import Control.Monad

import Control.Applicative (Applicative(liftA2))


main :: IO ()
main = do
  args <- getArgs
  let linesToRead = if length args > 0
                    then read (head args)
                    else 0 :: Int
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Integer]
  print $ sum ints

exampleMain :: IO ()
exampleMain = do
  vals <- mapM (\_ -> getLine) [1 .. 3]
  mapM_ putStrLn vals


-- Understanding traversable and sequence

traverseList :: Applicative f => (a -> f b) -> [a] -> f [b]
traverseList f l = res
  where lfs = map f l
        res = foldr (liftA2 (\el r -> el : r)) (pure []) lfs

sequenceList :: Applicative f => [f a] -> f [a]
sequenceList = foldr (liftA2 (\el r -> el : r)) (pure [])

traverseListViaSeq :: Applicative f => (a -> f b) -> [a] -> f [b]
traverseListViaSeq f = sequenceList . map f

-- Understanding applicative

-- ret :: Functor f => a -> f a
-- ret x = _

-- ap :: Functor f => f (a -> b) -> f a -> f b
-- ap  = _

-- lift2 :: Functor f => (a -> b -> c) -> f a -> f b -> f c
-- lift2 g fa fb = partiallyApplied `ap` fb
--   where partiallyApplied = fmap g fa

-- Why not Functor?
-- Possibly, because it won't be possible to perform an action in context with just fmap
replicateA :: Applicative m => Int -> m a -> m [a]
replicateA n ma = traverse (\_ -> ma) [1 .. n]
