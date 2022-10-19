module Problems.P0010 where

import Common.Math (primes)

{-
    The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

    Find the sum of all the primes below two million.

    ---

    The difficulty of these varies so wildly. Problem 9 felt like it would have been
    a lot easier if I was any good at math, but this one is yet another one I can solve
    in seconds with the knowledge that my implementation is about as good as it gets.
-}

-- >>> solve 2000000
-- 142913828922
solve :: Integer -> Integer
solve n = sum
    $ takeWhile (< n)
    primes
