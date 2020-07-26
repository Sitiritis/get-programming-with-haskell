data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 --deriving (Show)
  deriving (Eq, Ord, Enum)

instance Show SixSidedDie where
  show S1 = "I"
  show S2 = "II"
  show S3 = "III"
  show S4 = "IV"
  show S5 = "V"
  show S6 = "VI"

--instance Eq SixSidedDie where
--  (==) S6 S6 = True
--  (==) S5 S5 = True
--  (==) S4 S4 = True
--  (==) S3 S3 = True
--  (==) S2 S2 = True
--  (==) S1 S1 = True
--  (==) _ _ = False

--instance Enum SixSidedDie where
--  fromEnum S1 = 1
--  fromEnum S2 = 2
--  fromEnum S3 = 3
--  fromEnum S4 = 4
--  fromEnum S5 = 5
--  fromEnum S6 = 6
--
--  toEnum 1 = S1
--  toEnum 2 = S2
--  toEnum 3 = S3
--  toEnum 4 = S4
--  toEnum 5 = S5
--  toEnum 6 = S6
--  toEnum _ = error "No such value"

instance Bounded SixSidedDie where
  minBound = S1
  maxBound = S6

--instance Ord SixSidedDie where
--  compare a b = fromEnum a `compare` fromEnum b

--type Name = (String, String)
--data Name = Name (String, String) deriving (Show, Eq)
newtype Name = Name (String, String) deriving (Show, Eq)

names :: [Name]
names = [ Name ("Emil","Cioran")
        , Name ("Eugene","Thacker")
        , Name ("Friedrich","Nietzsche") ]

instance Ord Name where
  compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

data FiveSidedDie = SF1 | SF2 | SF3 | SF4 | SF5 deriving (Show, Enum)

class Die a where
  probability :: a -> Double

instance Die FiveSidedDie where
  probability die = 1 / 5

--mean = sum wightedValues
--  where values = map (\s -> fromIntegral (1 + fromEnum s)) [SF1 .. ]
--        wightedValues = map ((1 / 5) *) values
