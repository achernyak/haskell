data BookInfo = Book Int String [String]
  deriving (Show)

data MagazineInfo = Magazine Int String [String]
  deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

type CustomerID = Int
type ReviewBody = String

data BookReview = BookReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
  | CashOnDelivery
  | Invoide CustomerID
    deriving (Show)

bookID (Book id _ _) = id
bookTitle (Book id title authors) = title
bookAuthors (Book id title authors) = authors

--book and Customer acomplish the same thing.

data Customer = Customer {
  customerID :: CustomerID,
  customerName :: String,
  customerAddress :: Address
} deriving (Show)

customer1 = Customer 271828 "J.R. Hacker" [" 255 Syntax Ct", "Milpitas, Ca 95134", "USA"]
