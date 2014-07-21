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

span' f xs = (takeWhile f xs, dropWhile f xs)
break' f xs =
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
isInfix [] _ = True
isInfix _ [] = False
isInfix xs ya@(y:ys)
  | isPrefix xs ya = True
  | otherwise = isInfix xs ys

isSuffix :: (Eq a) => [a] -> [a] -> Bool
isSuffix xs ys = reverse xs `isPrefix` reverse ys

zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
zip' _ _ = []

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' _ _ _ = []

lines' :: String -> [String]
lines' "" = []
lines' str
  | suf == "\n" || suf == [] = [pre]
  | otherwise = pre: lines' (tail suf)
  where
    (pre,suf) = break (=='\n') str

safeListFunc :: ([b] -> a) -> [b] -> Maybe a
safeListFunc func [] = Nothing
safeListFunc func xs = Just (func xs)

safeHead = safeListFunc head
safeTail = safeListFunc tail
safeLast = safeListFunc last
safeInit = safeListFunc init

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f (x:xs) | not $ f x = splitWith f xs
splitWith f xs = (takeWhile f xs):(splitWith f next)
  where
    rest = dropWhile f xs
    next = dropWhile (not . f) rest
