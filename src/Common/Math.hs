module Common.Math where

import Prelude

import qualified Data.Map.Strict as Map

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

multiples :: (Enum a, Num a) => a -> [a]
multiples n = enumFromThen n (n * 2)

dividesBy :: Integral a => a -> a -> Bool
dividesBy i j = i `rem` j == 0

primes :: [Integer]
primes = 2 : sieve 3
    where
        sieve p =
            if isComposite p then
                sieve next
            else
                p : sieve next
            where
                next = p + 2

isComposite :: Integer -> Bool
isComposite c = any cDividesBy
    $ takeWhile inRange
    $ primes
    where
        inRange p = p * p <= c
        cDividesBy p = c `dividesBy` p

-- >>> factorise 100
-- [2,5,2,5]
factorise :: Integer -> [Integer]
factorise x =
    if factors == [] then
        [x]
    else if rest == 1 then
        factors
    else
        factors ++ (factorise rest)
    where
        factors = filter (x `dividesBy`)
            $ takeWhile halfXOrLess
            $ primes
        halfXOrLess p = p * 2 <= x
        rest = foldl div x factors

lowestCommonMultiple :: [Integer] -> Integer
lowestCommonMultiple xs = product unionCompositeFactors
    where
        factorCounts x = Map.fromListWith (+) $ fmap (\a -> (a, 1)) $ factorise x
        allFactorCounts = fmap factorCounts xs
        unionPrimeFactors = foldl (Map.unionWith max) Map.empty allFactorCounts
        unionCompositeFactors = fmap (uncurry (^)) $ Map.toList unionPrimeFactors