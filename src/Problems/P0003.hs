module Problems.P0003 where
    
import Common.Math (primes, dividesBy)
import Common.Lists (safeHead)

{-
    Problem:

    The prime factors of 13195 are 5, 7, 13 and 29.

    What is the largest prime factor of the number 600851475143 ?

    ---

    Pretty sure if there was a simple maths solution to this we wouldn't have modern crypto, so I won't even think about it.
-}

-- >>> largestPrimeFactor 600851475143
-- Just 6857
largestPrimeFactor :: Integer -> Maybe Integer
largestPrimeFactor n = safeHead
    $ filter (n `dividesBy`)
    $ reverse
    $ takeWhile (\p -> p * p <= n)
    primes
