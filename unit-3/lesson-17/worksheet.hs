import Data.Semigroup

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr (||) False . map p

data Color =
  Transparent |
  Red         |
  Yellow      |
  Blue        |
  Green       |
  Purple      |
  Orange      |
  Brown
  deriving (Show, Eq)

instance Semigroup Color where
  -- (<>) Red Blue = Purple
  -- (<>) Blue Red = Purple
  -- (<>) Yellow Blue = Green
  -- (<>) Blue Yellow = Green
  -- (<>) Yellow Red = Orange
  -- (<>) Red Yellow = Orange
  -- (<>) a b = if a == b
  --            then a
  --            else Brown
  (<>) Transparent c = c
  (<>) c Transparent = c
  (<>) a b | a == b = a
           | all (`elem` [Red,  Blue,   Purple]) [a, b] = Purple
           | all (`elem` [Blue, Yellow, Green])  [a, b] = Green
           | all (`elem` [Red,  Yellow, Orange]) [a, b] = Orange
           | otherwise = Brown

instance Monoid Color where
  mempty = Transparent

-- type Events = [String]
-- type Probs = [Double]

data Events = Events [String]
data Probs = Probs [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events (Probs probs) = PTable events (Probs normalizedProbs)
  where totalProbs = sum probs
        normalizedProbs = map (\x -> x / totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable (Events events) (Probs probs)) = mconcat rows
    where rows = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine f as bs = zipWith f repeatedAs cycledBs
  where numBs = length bs
        listRepeatedAs = map (take numBs . repeat) as
        repeatedAs = mconcat listRepeatedAs
        cycledBs = cycle bs
-- cartCombine f as bs = zipWith f newAs newBs
--   where prod = map (\x -> (x, bs)) as
--         cartProd = foldr (\(e, l) res -> res <> map (\x -> (e, x)) l) [] prod
--         newAs = map fst cartProd
--         newBs = map snd cartProd

-- combineEvents :: Events -> Events -> Events
-- combineEvents = cartCombine (\e1 e2 -> mconcat [e1, "-", e2])

-- combineProbs :: Probs -> Probs -> Probs
-- combineProbs = cartCombine (*)

instance Semigroup Events where
  (<>) (Events e1s) (Events e2s) = Events (cartCombine (\e1 e2 -> mconcat [e1, "-", e2]) e1s e2s)

instance Semigroup Probs where
  (<>) (Probs p1s) (Probs p2s) = Probs (cartCombine (*) p1s p2s)

instance Semigroup PTable where
  (<>) pt (PTable (Events []) (Probs [])) = pt
  (<>) (PTable (Events []) (Probs [])) pt = pt
  (<>) (PTable e1s p1s) (PTable e2s p2s) = createPTable newEvents newProbs
    where newEvents = e1s <> e2s
          newProbs = p1s <> p2s

instance Monoid PTable where
  mempty = PTable (Events []) (Probs [])

coin :: PTable
coin = createPTable (Events ["heads","tails"]) (Probs [0.5, 0.5])

spinner :: PTable
spinner = createPTable (Events ["red","blue","green"]) (Probs [0.1, 0.2, 0.7])