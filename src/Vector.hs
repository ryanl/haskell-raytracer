module Vector where

-- store scalars as floating-point doubles
type Scalar = Double

-- a vector is a list of scalars
data Vector = Vector [Scalar]

-- you can add, subtract, check equality of and multiply vectors term-by-term
instance Num Vector where
    (+) (Vector xs) (Vector ys) = Vector (zipWith (+) xs ys)
    (-) (Vector xs) (Vector ys) = Vector (zipWith (-) xs ys)
    (*) (Vector xs) (Vector ys) = Vector (zipWith (*) xs ys)
    negate (Vector xs) = Vector (map negate xs)
    fromInteger x = error "you probably didn't mean to do that"

instance Eq Vector where
    (==) (Vector xs) (Vector ys) = all id (zipWith (==) xs ys)

-- to convert a vector to a string, just convert its value list to string
instance Show Vector where
    show (Vector xs) = show xs

-- you can multiply all elements of the vector by a scalar
mult :: Scalar -> Vector -> Vector
mult l (Vector xs) = Vector (map (*l) xs)

normalise :: Vector -> Vector
normalise v@(Vector vs) = Vector (map (/mag) vs)
    where mag = (sqrt . vector_sum) (v * v)

-- get the sum of the scalar elements of a vector
vector_sum :: Vector -> Scalar
vector_sum (Vector xs) = sum xs

