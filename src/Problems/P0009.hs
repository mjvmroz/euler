module Problems.P0009 where

{-
    A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

        (a ^ 2) + (b ^ 2) = (c ^ 2)

    For example, (3 ^ 2) + (4 ^ 2)
               = 9 + 16
               = 25
               = (5 ^ 2).

    There exists exactly one Pythagorean triplet for which a + b + c = 1000.
    Find the product abc.
-}

-- >>> solve 1000
-- [31875000]
solve :: Integer -> [Integer]
solve n = take 1 [product | (a,b,c) <- pythagoreanTriples,
                            let sum = a + b + c,
                            let product = a * b * c,
                            n == sum]

-- >>> take 5 pythagoreanTriples
-- [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17)]
pythagoreanTriples :: [(Integer, Integer, Integer)]
pythagoreanTriples = [(a,b,c) | c <- [5..],
                                b <- [4..c-1],
                                a <- [3..b-1],
                                sq a + sq b == sq c]
    where
        sq = (^2)
