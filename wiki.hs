import Data.List

xor :: Bool -> Bool -> Bool
xor p q = (p ||q) && not (p && q)

cons8 list = 8 : list

--Simple I/O
rightArea base height = base * height / 2

main = do
  putStrLn "Enter your name:"
  name <- getLine
  if name == "John" || name == "Phil"
     then putStrLn "Haskell is Great"
     else if name == "Koan"
      then putStrLn "Debugging is Great"
      else putStrLn "I don't know you"     

--Recursion
factorial 0 = 1
factorial n = n * factorial (n - 1)

doubleFactorial 0 = 1
doubleFactorial 1 = 1
doubleFactorial n = n * factorial (n - 2)

power x 0 = 1
power x y = x * power x (y-1)

plusOne x = x + 1
addition x 0 = x
addition x y = plusOne (addition x (y - 1))

log2 1 = 0
log2 n = 1 + log2 (n `div` 2)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' x y = y : replicate (x - 1) y

ind :: [a] -> Int -> a
ind (x:_) 0 = x
ind (x:xs) n = ind xs (n-1)

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

length' xs = go 0 xs
  where
  go acc [] = acc
  go acc (_:xs) = go (acc + 1) xs

--Morea about lists
takeInt :: Int -> [a] -> [a]
takeInt 0 _ = []
takeInt _ [] = []
takeInt n (x:xs) = x : takeInt (n-1) xs

dropInt :: Int -> [a] -> [a]
dropInt 0 list = list
dropInt _ [] = []
dropInt n (x:xs) = dropInt (n-1) xs

scanInt :: [Int] -> Int
scanInt [] = 0
scanInt (x:xs) = x + scanInt xs

scanSum :: [Int] -> [Int]
scanSum [] = []
scanSum (a:[]) = a : []
scanSum (a:b:as) = a : scanSum ((a+b):as)

diffs :: [Int] -> [Int]
diffs [] = []
diffs (x:[]) = []
diffs (x:y:xs) = (y-x) : diffs (y:xs)

negateList :: [Int] -> [Int]
negateList list = map negate list

divisors p = [ f | f <- [1..p], p `mod` f == 0 ]

divisorList :: [Int] -> [[Int]]
divisorList list = map divisors list

negateDivisor :: [Int] -> [[Int]]
negateDivisor list = map (negateList . divisors) list

encode :: [Char] -> [(Int, Char)]
encode list = map go (group list)
  where
  go (x:xs) = (length (x:xs) , x)

decode :: [(Int, Char)] -> [Char]
decode list = concatMap go list
  where
  go (l, c) = replicate l c
lastOf :: [a] -> a
lastOf [] = error "Empty list"
lastOf (x:[]) = x
lastOf (x:xs) = lastOf xs

dropLast :: [a] -> [a]
dropLast [] = error "Empty list"
dropLast (x:[]) = []
dropLast (x:xs) = x : dropLast xs

--List Processing
scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f a [] = [a]
scanr' f a (x:xs) = (f x (head prev)):prev
  where prev = scanr' f a xs

scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f a [] = [a]
scanl' f a (x:xs) = a : scanl' f (f a x) xs

scanr'' :: (a -> b -> b) -> b -> [a] -> [b]
scanr'' f a xs = foldr f' [a] xs
  where f' x xs = (f x (head xs)):xs

factList :: Integer -> [Integer]
factList n = scanl1 (*) [1..n]

returnDivisible :: Int -> [Int] -> [Int]
returnDivisible n xs = [ x | x <- xs, (mod x n) == 0]
returnDivisible' n = filter (\x -> (mod x n) == 0)

randf :: [[Int]] -> [[Int]]
randf xs = [ tail x | x <- xs, not (null x), head x > 5 ]

minusTen :: (Floating a) => a -> a
minusTen = (-) 10
