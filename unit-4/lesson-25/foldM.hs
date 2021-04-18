{-# LANGUAGE ScopedTypeVariables #-}

mono :: a -> b -> c -> (a, a)
mono x y z = (f x y, f x z)
  where
    -- b is not implicitly universally quantified because it is in scope
    f :: a -> b -> a
    f x' _ = x'

mkpair1 :: a -> b -> (a, b)
mkpair1 aa bb = (ida aa, bb)
    where
      ida :: a -> a -- This refers to a in the function's type signature
      ida = id

testId :: forall a. a -> a -- Note: forall is necessary here to declare type variables
testId val = val
  where
    res :: a
    res = val -- for some mysterious does not work

test :: forall m b. (Monad m) => b -> m b -- Note: forall is necessary here to declare type variables
test val = liftedVal
  where
    liftedVal :: m b
    liftedVal = return val -- for some mysterious does not work

foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM f z0 xs = foldrc return xs z0
  -- See Note [List fusion and continuations in 'c']
  where 
        -- c :: a -> (b -> m b) -> b -> m b
        c x k z = f z x >>= k
        {-# INLINE c #-}
        foldrc = foldr c

myFoldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
myFoldM f z t = foldl c (return z) t
  where
    c mx y = do
      x <- mx
      f x y
