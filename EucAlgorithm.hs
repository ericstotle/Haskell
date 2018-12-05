-- the euclidean algorithm in haskell. more content underway.

gdc :: Int -> Int -> Int
gdc x y | x == y = x
        | x > y = gdc (x - y) y
        | x < y = gdc x (y - x)
