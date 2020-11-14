module Exercises.Foldr where

{-
    Exercises from list-folds.pdf
-}

-- >>> append [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]
append :: [a] -> [a] -> [a]
append x y = foldr (:) y x

-- >>> map' (+1) [1,2,3,4,5,6]
-- [2,3,4,5,6,7]
map' :: (a -> b) -> [a] -> [b]
map' f = foldr transform []
    where
        transform = (:) . f

-- >>> filter' odd [1,2,3,4,5,6]
-- [1,3,5]
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr transform []
    where
        transform a =
            if p a then (:) a else id

-- >>> concat' [[1,2], [3,4], [5,6]]
-- [1,2,3,4,5,6]
concat' :: [[a]] -> [a]
concat' = foldr append []

-- >>> concatMap' (\x -> [x+1]) [1,2,3,4,5,6]
-- [2,3,4,5,6,7]
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = foldr transform []
    where transform = append . f

-- >>> count' [1,2,3,4,5]
-- 5
count' :: [a] -> Integer
count' = foldl (+) 0 . fmap (const 1)