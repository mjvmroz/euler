module Common.Lists where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead as = Just $ head as

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast as = Just $ last as

find :: (a -> Bool) -> [a] -> Maybe a
find p c = safeHead $ filter p c
