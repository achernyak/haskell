data Tree a = Node a (Tree a) (Tree a)
  | Emptry
    deriving (Show)
    
