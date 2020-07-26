-- data AuthorName = AuthorName {
--     firstName :: String
--   , lastName  :: String
--   } deriving (Show)
-- data Book = Book {
--     author :: AuthorName
--   , isbn   :: String
--   , title  :: String
--   , year   :: Int
--   , price  :: Double
--   } deriving (Show)
type FirstName  = String
type MiddleName = String
type LastName   = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
  | FirstNameWithTwoInits FirstName Char Char
  deriving (Show)

data Author = Author Name
  deriving (Show)

data Artist = Person Name | Band String
  deriving (Show)

data Creator = AuthorCreator Author | ArtistCreator Artist
  deriving (Show)

hpLovecraft :: Creator
hpLovecraft = AuthorCreator $ Author $ TwoInitialsWithLast 'H' 'P' "Lovecraft"

data Book = Book {
    author    :: Creator
  , isbn      :: String
  , bookTitle :: String
  , bookYear  :: Int
  , bookPrice :: Double
  }
  deriving (Show)

data VinylRecord = VinylRecord {
    artist      :: Creator
  , recordTitle :: String
  , recordYear  :: Int
  , recordPrice :: Double
  }
  deriving (Show)

data CollectibleToy = CollectibleToy {
    name        :: String
  , toyDescription :: String
  , toyPrice    :: Double
  }
  deriving (Show)

data Contacts = Contacts {
    companyName    :: String
  , companyAddress :: String
  , companyPhone   :: String
  , companyEmail   :: String
  }
  deriving (Show)

data Pamphlet = Pamphlet {
    pamphletTitle       :: String
  , pamphletDescription :: String
  , pamphletContacts    :: Contacts
  }
  deriving (Show)

data StoreItem
  = BookItem Book
  | RecordItem VinylRecord
  | ToyItem CollectibleToy
  | PamphletItem Pamphlet
  deriving (Show)

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy _ = "Unknown"

-- Shapes

data Circle = Circle {
    radius :: Double
  }

data Square = Square {
    sideLen :: Double
  }

data Rectangle = Rectangle {
    leftLen :: Double
  , topLen  :: Double
  }

data Shape
  = CircleShape Circle
  | SquareShape Square
  | RectangleShape Rectangle

perimeter :: Shape -> Double
perimeter (CircleShape circle) = 2 * pi * radius circle
perimeter (SquareShape square) = 4 * sideLen square
perimeter (RectangleShape rect) = 2 * leftLen rect + 2 * topLen rect

area :: Shape -> Double
area (CircleShape circle) = pi * (radius circle) ^ 2
area (SquareShape square) = (sideLen square) ^ 2
area (RectangleShape rect) = leftLen rect * topLen rect
