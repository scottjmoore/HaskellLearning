doubleMe x = x + x
doubleUs x y = x + x + y + y
doubleSmallNumber x = if x > 100
    then x
    else x + x

doubleSmallNumber' x = (if x > 100 then x else x + x) + 1

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial(x - 1)

sayMe :: Int -> String
sayMe 0 = "Zero"
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe 6 = "Six"
sayMe 7 = "Seven"
sayMe 8 = "Eight"
sayMe 9 = "Nine"
sayMe _ = "Lots!!!!"

addVector3d :: (Num a) => (a, a, a) -> (a, a, a) -> (a, a, a)
addVector3d (x, y, z) (x', y', z') = (x + x', y + y', z + z')

primes = filterPrime [2..]
    where filterPrime (p:xs) =
            p : filterPrime [x | x <- xs, mod x p /= 0]