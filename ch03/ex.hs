import Data.List (sortBy)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

total :: [Double] -> Double
total [] = 0.0
total (x:xs) = x + total xs

mean :: [Double] -> Double
mean [] = 0
mean (x:xs) = (total xs) / fromIntegral (length xs)

pali :: [a] -> [a]
pali [] = []
pali (x:xs) = [x] ++ pali xs ++ [x]

isPali :: (Eq a) => [a] -> Bool
isPali xs = (xs == reverse xs)


sortByLength :: [[a]] -> [[a]]
sortByLength xs = sortBy compareLength xs
  where compareLength l1 l2 = compare (length l1) (length l2)

intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ [x] = x
intersperse sep (x:xs) = x ++ [sep] ++ intersperse sep xs

data Tree a = Node a (Tree a) (Tree a)
  | Empty
    deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ t1 t2) = 1 + max (treeHeight t1) (treeHeight t2)

data Direction = LeftTurn | RightTurn | Straight
  deriving (Show)

data Point = Point {x :: Double, y :: Double}
  deriving (Show)

turn :: Point -> Point -> Point -> Direction
turn p1 p2 p3
  | prod > 0 = LeftTurn
  | prod < 0 = RightTurn
  | otherwise = Straight
  where prod = ((x(p2) - x(p1)) * (y(p3) - y(p1))) - ((y(p2) - y(p1)) * (x(p3) - x(p1)))

directions :: [Point] -> [Direction]
directions ps | length ps < 3 = []
directions (a:b:c:ps) = turn a b c : directions (a:b:ps)
