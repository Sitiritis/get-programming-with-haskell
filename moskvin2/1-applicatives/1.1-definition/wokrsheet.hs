newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}
newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

instance Functor (Arr2 e1 e2) where
  fmap f (Arr2 r2) = Arr2 (\env1 env2 -> f $ r2 env1 env2)

instance Functor (Arr3 e1 e2 e3) where
  fmap f (Arr3 r3) = Arr3 (\env1 env2 env3 -> f $ r3 env1 env2 env3)


{-

>>> getArr2 (fmap length (Arr2 take)) 10 "abc"
3

>>> getArr3 (tail <$> tail <$> Arr3 zipWith) (+) [1,2,3,4] [10,20,30,40,50]
[33,44]

-}

-- Prove `fmap f (fmap g xs) = fmap (f . g) xs` for [a]

-- Base case
prop_secondFunctorLawForList f g [] =
  fmap f (fmap g []) == fmap (f . g) []

{-

(1) fmap f (fmap g [])
  -- def map
  = fmap f []
  -- def map
  = []

(2) fmap (f . g) []
  -- def map
  = []

||
\/

(1) = (2)

-}

-- Inductive step
prop_secondFunctorLawForList f g (x : xs) =
  fmap f (fmap g (x : xs)) == x : fmap (f . g) (x : xs)

{-

Assume `fmap f (fmap g xs) = fmap (f . g) xs` holds for `xs`, then

(1) fmap f (fmap g (x : xs))
  -- def map
  = fmap f (g x : fmap g xs)
  -- def map
  = f g x : fmap f (fmap g xs)
  -- by assumption
  = f g x : fmap (f . g) xs

(2) fmap (f . g) (x : xs)
  -- def map
  = (f . g) x : fmap (f . g) xs
  -- def (.)
  = f g x : fmap (f . g) xs

||
\/

(1) = (2)

||
\/

The second functor law holds for [a] functor.

-}


data Triple a = Tr a a a deriving (Eq, Show)

instance Functor Triple where
  fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
  pure v = Tr v v v
  Tr fx fy fz <*> Tr x y z = Tr (fx x) (fy y) (fz z)

{-

>>> (^2) <$> Tr 1 (-2) 3
Tr 1 4 9

>>> Tr (^2) (+2) (*3) <*> Tr 2 3 4
Tr 4 5 12

-}
