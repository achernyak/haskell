data List a = Cons a (List a)
  | Nil
    deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

toList (Cons a as) = a : toList as
toList Nil = []
