module Problems.P0001 where

{-
    Problem:

    If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
    Find the sum of all the multiples of 3 or 5 below 1000.

    ---

    Solution:

    sum(multiples(i)) + sum(multiples(j)) - sum(multiples(i . j))
-}

-- >>> solve 1000 3 5
-- 233168
solve :: Integral a => a -> a -> a -> a
solve n = solveInclusive $ n - 1

solveInclusive :: Integral a => a -> a -> a -> a
solveInclusive n i j = sumI + sumJ - sumIJ
    where
        sumI = sumMultiples n i
        sumJ = sumMultiples n j
        sumIJ = sumMultiples n (i * j)

-- Sum of multiples of i in n
sumMultiples :: Integral a => a -> a -> a
sumMultiples n i = (doubleAverageMultiple * multiplesCount) `div` 2
    where
        -- The number of multiples(i) <= n
        multiplesCount = n `div` i
        -- The largest value in multiples(i) <= n
        lastMultiple = n - (n `mod` i)
        -- Double the average of multiples(i) <= n.
        doubleAverageMultiple = lastMultiple + i
