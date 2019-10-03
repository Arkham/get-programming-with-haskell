module Store where

type FirstName = String

type MiddleName = String

type LastName = String

data Name
  = Name FirstName
         LastName
  | NameWithMiddle FirstName
                   MiddleName
                   LastName
  | TwoInitialsWithLast Char
                        Char
                        LastName
  deriving (Eq, Show)

newtype Author =
  Author Name
  deriving (Eq, Show)

data Artist
  = Person Name
  | Band String
  deriving (Eq, Show)

data Creator
  = AuthorCreator Author
  | ArtistCreator Artist
  deriving (Eq, Show)

data Book = Book
  { author :: Creator
  , isbn :: String
  , bookTitle :: String
  , bookYear :: Int
  , bookPrice :: Double
  } deriving (Eq, Show)

data VinylRecord = VinylRecord
  { artist :: Creator
  , recordTitle :: String
  , recordYear :: Int
  , recordPrice :: Double
  } deriving (Eq, Show)

data CollectibleToy = CollectibleToy
  { name :: String
  , descrption :: String
  , toyPrice :: Double
  } deriving (Eq, Show)

data Pamphlet = Pamphlet
  { title :: String
  , description :: String
  , contact :: String
  } deriving (Eq, Show)

data StoreItem
  = BookItem Book
  | RecordItem VinylRecord
  | ToyItem CollectibleToy
  | PamphletItem Pamphlet
  deriving (Eq, Show)

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0.0

-- example
hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

mountainsOfMadness :: Book
mountainsOfMadness =
  Book
    { author = hpLovecraft
    , isbn = "1234"
    , bookTitle = "Mountains of Madness"
    , bookYear = 1850
    , bookPrice = 11.50
    }

store :: [StoreItem]
store = [BookItem mountainsOfMadness]
