module Main where

main :: IO ()
main = putStr
    . concat
    . map printOne
    . index
    $ commonElements ( index triangles ) ( index squares )

-----------
-- TYPES --
-----------

-- These make things a bit more readable
newtype Index = Index Integer deriving (Show, Eq, Ord)
type Indexed a = (Index, a)
type IndexedList a = [Indexed a]

------------------------------------------------
-- FUNCTIONS FOR MANIPULATING DATA STRUCTURES --
------------------------------------------------

-- Label all elements of a list with their index
index :: [a] -> IndexedList a
index = zip ( map Index [ 0 .. ] )

-- Given an indexed element, get its index
indexOf :: Indexed a -> Index
indexOf (idx, _) = idx

-- Given an indexed element, get its value
valueOf :: Indexed a -> a
valueOf (_, a) = a

listAllPositive :: (Integer -> Integer) -> [Integer]
listAllPositive f = map f [ 0 .. ]

---------------------------
-- THE INTERESTING STUFF --
---------------------------

-- Calculate the nth square number
square :: Integer -> Integer
square n = n * n

-- A list of all the square numbers
squares :: [Integer]
squares = listAllPositive square

-- Calculate the nth triangle number
triangle :: Integer -> Integer
triangle n = (n * (n - 1)) `div` 2

-- A list of all the triangle numbers, Start
triangles :: [Integer]
triangles = tail $ listAllPositive triangle

-- Explain a tuple with index, triangle-square number, the triangle root and
-- the square root
printOne :: Indexed (Integer, Index, Index) -> String
printOne (Index i, (n, Index t, Index s)) = concat [ show i, ": ", show n,
    " is the ", show t, "th triangle and ", show s, " squared\n" ]

-- Given two ordered indexed lists, find elements that are found in both lists
-- and their indices in each list
commonElements :: Ord a => IndexedList a -> IndexedList a ->
    [(a, Index, Index)]
commonElements []     _      = []
commonElements _      []     = []
commonElements (a:as) (b:bs) =
    if valueOf a < valueOf b then commonElements as (b:bs) else
    if valueOf a > valueOf b then commonElements (a:as) bs else
    (valueOf a, indexOf a, indexOf b) : commonElements as bs
