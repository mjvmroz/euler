module Problems.P0005 where

import Common.Math (lowestCommonMultiple)

{-
    Problem:

    2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

    What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

-}

-- >>> solution 20
-- 232792560
solution x = lowestCommonMultiple [1..x]