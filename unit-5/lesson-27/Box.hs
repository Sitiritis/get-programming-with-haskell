{-# LANGUAGE OverloadedStrings #-}

module Box where

newtype Box a = Box a deriving Show

instance Functor Box where
  fmap func (Box x) = Box $ func x

morePresents :: Int -> Box a -> Box [a]
morePresents n = fmap $ replicate n

myBox :: Box Int
myBox = Box 1

wrapped :: Box (Box Int)
wrapped = Box <$> myBox

unwrap :: Box a -> a
unwrap (Box x) = x
