{-# LANGUAGE TypeOperators #-}
import GHC.Base (Applicative)

infixr 9 |.|
newtype (|.|) f g a = Cmps {getCmps :: f (g a)}
  deriving (Eq, Show)

type A = ((,) Integer |.| (,) Char) Bool

a :: A
a = Cmps (1, ('a', True))

type B t = ((,,) Bool (t -> t) |.| Either String) Int

b :: B t
b = Cmps (True, id, Right 1)

type C = (|.|) ((->) Bool) ((->) Integer) Integer

c :: C
c = Cmps $ \b i -> i

instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap h (Cmps fga) = Cmps $ fmap (fmap h) fga

newtype Cmps3 f g h a = Cmps3 {getCmps3 :: f (g (h a))}
  deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
  fmap i (Cmps3 fgha) = Cmps3 $ fmap (fmap (fmap i)) fgha

{-
(1) fmap id == id

fmap id (Cmps x)           -- def fmap (Cmps)
== Cmps $ fmap (fmap id) x -- (1) fmap (g)
== Cmps $ fmap id x        -- (1) fmap (f)
== Cmps $ id x
== Cmps x
-}

{-
(2) fmap h2 (fmap h1 (Cmps x)) = fmap (h2 . h1) (Cmps x)
Or, equivalently
(2) (fmap h2) . (fmap h1) = fmap (h2 . h1)

fmap h2 (fmap h1 (Cmps x)) -- def fmap (Cmps)
== fmap h2 (Cmps $ fmap (fmap h1) x) -- def fmap (Cmps)
== Cmps $ fmap (fmap h2) (fmap (fmap h1) x) -- (2) fmap (g)
== Cmps $ fmap ((fmap h2) . (fmap h1)) x -- (2) fmap (f)
== Cmps $ fmap (fmap (h2 . h1)) x -- def fmap (Cmps)
== fmap (h2 . h1) (Cmps x)
-}

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
  pure = Cmps . pure . pure
  Cmps fgh <*> Cmps fgx = Cmps $ fmap (<*>) fgh <*> fgx

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 = fmap (fmap getCmps) getCmps

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 = fmap (fmap getCmps . getCmps) . getCmps
-- unCmps4 cmps = fmap (fmap getCmps . getCmps) (getCmps cmps)

{-

>>> pure 42 :: ([] |.| [] |.| []) Int
>>> unCmps3 (pure 42 :: ([] |.| [] |.| []) Int)
>>> unCmps3 (pure 42 :: ([] |.| Maybe |.| []) Int)
>>> unCmps4 (pure 42 :: ([] |.| [] |.| [] |.| []) Int)
Cmps {getCmps = [Cmps {getCmps = [[42]]}]}
[[[42]]]
[Just [42]]
[[[[42]]]]

-}
