{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

--Problem 1
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

prop_myLast xs = length xs > 0 ==> myLast xs == last xs

--Problem 2
myLastBut :: [a] -> a
myLastBut = last . init

--Problem 3
elementat :: [a] -> Int -> a
elementat (x:_) 1 = x
elementat (x:xs) i = elementat xs (i - 1)

--Problem 4
myLength :: [a] -> Int
myLength xs = 
  let len [] n = n
      len (_:xs) n = len xs (n + 1)
  in  len xs 0

myLength' = foldr (\_ n -> n + 1) 0

prop_myLength' xs = myLength' xs == length xs
prop_myLength xs = myLength xs == length xs

--Problem 5
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

proprmyRevers xs = myReverse xs == reverse xs

--Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = reverse xs == xs

--Problem 8
compress :: (Eq a) => [a] -> [a]
compress xs = foldr (\x acc -> if x == (head acc) then acc else x:acc) [last xs] xs 

--Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (first, rest) = span (==x) xs
                  in (x:first) : pack rest

--Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = let tup x = (length x, head x)
             in map tup (pack xs)

encode' :: (Eq a) => [a] -> [(Int, a)]
encode' xs = map (\x -> (length x, head x)) (pack xs)

main = $(quickCheckAll)

--Problem 11
data ListItem a = Single a | Multiple Int a
  deriving (Show)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified =
  let encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x
  in  map encodeHelper . encode

--Problem 12
decode :: [ListItem a] -> [a]
decode = 
  let decode' (Single x) = [x]
      decode' (Multiple n x) = replicate n x
  in  concatMap decode'

--Promble 13
encodeDirect :: Eq a => [a] -> [ListItem a]

encodeDirect [] = []
encodeDirect (x:xs)
  | count==1 = (Single x) : (encodeDirect xs)
  | otherwise = (Multiple count x) : (encodeDirect rest)
  where
    (matched, rest) = span (==x) xs
    count = 1 + (length matched)

--Problem 14
dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

dupli' :: [a] -> [a]
dupli' (x:xs) = x:x:dupli' xs

--Problem 15
repli :: [a] -> Int -> [a]
repli = flip $ concatMap . replicate

--Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery list n = 
  let every [] _ _ = []
      every (x:xs) ns 1 = every xs ns ns
      every (x:xs) ns n = x : every xs ns (n - 1)
  in  every list n n

--Problem 17
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

--Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = (l, x:r)
  where (l, r) = removeAt (n - 1) xs

--Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x ys 1 = x:ys
insertAt x (y:ys) n = y:insertAt x ys (n - 1)

--Problem 22
range :: (Ord a, Enum a) => a -> a -> [a]
range a b | (a == b) = [a]
range a b = a:range ((if a < b then succ else pred) a) b  

--Problem 31
isPrime :: Integral a => a -> Bool
isPrime x = null $ filter divisible $ takeWhile notTooBig [2..]
  where
  divisible y = x `mod` y == 0
  notTooBig y = y*y <= x

--Problem 32
coprime :: Integral a => a -> a -> Bool
coprime a b = gcd a b == 1

--Problem 34
totient 1 = 1
totient n = length $ filter (coprime n) [1..n-1]

--Problem 35
factor :: Integer -> [Integer]
factor 1 = []
factor n = let prime = head $ dropWhile ((/= 0) . mod n) [2..n]
               in (prime :) $ factor $ div n prime 
