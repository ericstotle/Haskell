-- a Caesar cipher from ch.5 of Hutton's Programming in Haskell (2016)

import Data.Char

--ceasar cypher
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
            | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x (zip xs [0..n])
                where n = (length xs) - 1

--frequency tables
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
  where
   factor = head (positions (minimum chitab) chitab)
   chitab = [chisqr (rotate n table') table | n <- [0..25]]
   table' = freqs xs

grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int,Int)]
square p = [(x,y) | (x,y) <- grid p p, x /= y]

plicate :: Int -> a -> [a]
plicate n x = [x | _ <- [0..n]]

thags :: Int -> [(Int,Int,Int)]
thags n = [(x,y,z) | x <- [1..n],
                     y <- [1..n],
                     z <- [1..n],
                     x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum(factors x) - x]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

expo :: Int -> Int -> Int
expo n 0 = 1
expo n x = n * expo n (x-1)

euclid :: Int -> Int -> Int
euclid x y | x == y        = x
           | x > y         = euclid y (x-y)
           | x < y         = euclid x (y-x)

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]

wand :: [Bool] -> Bool
wand [] = True
wand (x:xs) = if x == True then wand xs else False

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x : xs) = x ++ concat' xs -- ++ is list concatenator

-- produce a list with n identical elements:
replicate' :: Int -> a -> [a]
replicate' 0 _ = []                -- _ means any entry. 0# of any _'s is a list [].
replicate' n x = x : replicate' (n-1) x

-- select the nth item of list
(!!!) :: [a] -> Int -> a
xs !!! 0 = head xs
xs !!! n = tail xs !!! (n-1) {-|

-}

ele :: Eq a => a -> [a] -> Bool
ele _ [] = False
ele n xs = if n == head xs then True else
         ele n (tail xs)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = x : y : merge xs ys
