module Problems.P0004 where

{-
    Problem:

    A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

    Find the largest palindrome made from the product of two 3-digit numbers.
-}

-- >>> solve 3
-- 906609
solve :: (Integral b, Show a, Num a, Ord a) => b -> a
solve digits = maximum $ productPalindromes digits

productPalindromes :: (Integral b, Show a, Ord a, Num a) => b -> [a]
productPalindromes digits = search max max
    where
        min = 10 ^ (digits - 1)
        max = (10 ^ digits) - 1
        search x y =
            if isPalindrome product then
                product : rest
            else
                rest
            where
                product = x * y
                rest = if y > x then
                    search x (y - 1)
                else if x > min then
                    search (x - 1) max
                else
                    []

isPalindrome :: Show a => a -> Bool
isPalindrome i = str == reverse str
    where
        str = show i
