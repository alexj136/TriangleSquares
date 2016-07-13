module TriangleSquares where

main :: IO ()
main = putStr
    . concat
    . map printOne
    $ computeTrackingSeeds ( zip triangles [ 1 .. ] ) ( zip squares [ 1 .. ] ) 1

-- Calculate the nth square number
square :: Integer -> Integer
square n = n * n

-- A list of all the square numbers
squares :: [Integer]
squares = [ n * n | n <- [ 1 .. ] ]

-- A list of all the triangle numbers
triangles :: [Integer]
triangles = [ triangle n | n <- [ 1 .. ] ]

-- Calculate the nth triangle number
triangle :: Integer -> Integer
triangle n | n < 1 = undefined
triangle 1         = 1
triangle n         = n + triangle (n - 1)

-- Explain a tuple with index, triangle-square number, the triangle root and
-- the square root
printOne :: (Integer, Integer, Integer, Integer) -> String
printOne (i, n, t, s) = show i ++ ": " ++ show n ++ " is the " ++ show t ++
    "th triangle and " ++ show s ++ " squared\n"

-- Main computation
computeTrackingSeeds :: Ord a =>
    [(a,a)] -> [(a,a)] -> Integer -> [(Integer,a,a,a)]
computeTrackingSeeds []     _      i          = []
computeTrackingSeeds _      []     i          = []
computeTrackingSeeds ((a, ga):as) ((b, gb):bs) i | a <  b =
    computeTrackingSeeds as ((b, gb):bs) i
computeTrackingSeeds ((a, ga):as) ((b, gb):bs) i | a >  b =
    computeTrackingSeeds ((a, ga):as) bs i
computeTrackingSeeds ((a, ga):as) ((b, gb):bs) i | a == b =
    (i, a, ga, gb) : computeTrackingSeeds as bs (i + 1)
