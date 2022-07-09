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

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z

addVector3 :: (Num a) => (a, a, a) -> (a, a, a) -> (a, a, a)
addVector3 (x, y, z) (x', y', z') = (x + x', y + y', z + z')

primes = filterPrime [2..]
    where
        filterPrime (p:xs) =
            p : filterPrime [x | x <- xs, mod x p /= 0]

sign :: (Integral a) => a -> String
sign x
    | y < zero = minusStr
    | y > zero = positiveStr
    | otherwise = zeroStr
    where
        y = x
        zero = 0
        (minusStr, zeroStr, positiveStr) = ("-", "0", "+")

doTwice :: (a -> a) -> a -> a
doTwice f x = f (f x)

half :: (Integral a) => a -> a
half x = div x 2

addTimesOneHundered :: (Num a) => a -> a -> a
addTimesOneHundered x y = x + (y * 100)

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let leftSort = quickSort (filter (<=x) xs)
        rightSort = quickSort (filter (>x) xs)
    in  leftSort ++ [x] ++ rightSort

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (div n 2)
    | odd n = n:chain(n*3 + 1)

numLongChains :: Int
numLongChains = length $ filter isLong $ map chain [1..100]
    where isLong xs = length xs > 15

testFold :: (Num a) => [a] -> a
testFold xs = foldl fold 0 xs
    where fold = (+)

mathProb :: (Floating a) => a -> a
mathProb x = (5 ** x) - (3 ** x)

--mathProb2 :: Floating -> Floating
mathProb2 x = ((16 ** x) + (20 ** x))
