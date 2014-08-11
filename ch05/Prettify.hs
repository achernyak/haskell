module Prettify where

data Doc = Empty
	| Char Char
	| Text String
	| Line 
	| Concat Doc Doc
	| Union Doc Doc
		deriving (Show)

empty :: Doc
empty = Empty

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double num = text (show num)

line :: Doc
line = Line

char :: Char -> Doc
char = Char

hcat :: [Doc] -> Doc
hcat = fold (<>)

fsep :: [Doc] -> Doc
fsep = fold (</>)

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate _ [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

compact :: Doc -> String
compact x = transform [x]
	where 
		transform [] = ""
		transform (d:ds) =
			case d of
				Empty -> transform ds
				Char c -> c : transform ds
				Text s -> s ++ transform ds
				Line -> '\n' : transform ds
				a `Concat` b -> transform (a:b:ds)
				_ `Union` b -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
	where 
		best col (d:ds) =
			case d of
				Empty -> best col ds
				Char c -> c : best (col + 1) ds
				Text s -> s ++ best (col + length s) ds
				Line -> '\n' : best 0 ds
				a `Concat` b -> best col (a:b:ds)
				a `Union` b -> nicest col (best col (a:ds)) (best col(b:ds))
		
		best _ _ = ""

		nicest col a b
			| (width - least) `fits` a = a
			| otherwise = b
			where least = min width col

fits :: Int -> String -> Bool 
w `fits` _ | w < 0 = False
_ `fits` "" = True
_ `fits` ('\n':_) = True
w `fits` (_:cs) = (w - 1) `fits` cs

nest :: Int -> Doc -> Doc
nest tab doc = indent 0 [doc]
	where
		indent lvl (d:ds) =
			case d of
				Empty -> indent lvl ds
				Text s -> text s <> indent lvl ds
				Char '{' -> char '{' <> indent (lvl + 1) (line:ds)
				Char '[' -> char '[' <> indent (lvl + 1) (line:ds)
				Char '}' -> char '}' <> indent (lvl - 1) ds
				Char ']' -> char ']' <> indent (lvl - 1) ds
				Char c -> char c <> indent lvl ds
				Line -> line <> text (tabs ds) <> indent lvl ds
					where
						tabs (x:_) =
							case x of
								Char '}' -> replicate (tab * (lvl - 1)) ' '
								Char ']' -> replicate (tab * (lvl - 1)) ' '
								_ -> replicate (tab * lvl) ' '
						tabs _ = replicate (tab * lvl) ' '
				a `Concat` b -> indent lvl (a:b:ds)
				a `Union` b -> indent lvl (a:ds) `Union` indent lvl (b:ds)
		indent _ _ = empty