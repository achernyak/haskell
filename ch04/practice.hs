concat' :: [[a]] -> [a]
concat' (x:xs) = x ++ concat' xs
concat' _ = []

reverse' [] = []
reverse' xs = foldl (\x y -> y:x) [] xs

and' :: [Bool] -> Bool
and' (True:xs) = and' xs
and' (False:_) = False
and' _ = True

and'' = foldl (&&) True
or' = foldl (||) False

all' f = and . map f

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = x:take' n xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f (x:xs) = if f x
  then x:takeWhile' f xs
  else []

span f xs = (takeWhile f xs, dropWhile f xs)
break f xs =
  let g = not . f
  in (takeWhile g xs, dropWhile g xs)

elem' :: (Eq a) => a -> [a] -> Bool
elem' x = not . null . dropWhile (/= x)

notElem' x = not. elem' x

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x:filter' f xs
  | otherwise = filter' f xs

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix xs ys = xs == take (length xs) ys

isInfix :: (Eq a) => [a] -> [a] -> Bool
isInfix _ [] = False
isInfix xs ys
  | xs == zs = True
  | otherwise = isInfix xs zs
  where zs = take (length xs) ys
