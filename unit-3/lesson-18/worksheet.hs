import qualified Data.Map as Map
import qualified Data.Tuple as Tuple

data Box a = Box a deriving Show

wrap :: a -> Box a
wrap = Box

unwrap :: Box a -> a
unwrap (Box x) = x

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box (f x)

data Triple a = Triple a a a deriving Show

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

transform :: (a -> a) -> Triple a -> Triple a
transform = tripleMap

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

type Initials = Triple Char

initials :: Initials
initials = Triple 'H' 'P' 'L'

data List a = Empty | Cons a (List a) deriving Show

builtinEx1 :: [Int]
builtinEx1 = 1 : 2 : 3 : []
ourListEx1 :: List Int
ourListEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

builtinEx2 :: [Char]
builtinEx2 = 'c' : 'a' : 't' : []
ourListEx2 :: List Char
ourListEx2 = Cons 'c' (Cons 'a' (Cons 't' Empty))

ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap f (Cons x xs) = Cons (f x) (ourMap f xs)

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

organInventory :: Map.Map Organ Int
organInventory = Map.fromList (zip allOrgans counts)
  where organs = map snd (Map.toList organCatalog)
        allOrgans = [Heart ..]
        counts = map (\o -> length $ filter (== o) organs) allOrgans