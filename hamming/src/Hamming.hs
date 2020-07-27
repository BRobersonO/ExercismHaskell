module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys = distance' xs ys 0

distance' :: (Eq a1, Num a2) => [a1] -> [a1] -> a2 -> Maybe a2
distance' [] [] num = Just num
distance' [] _ _= Nothing
distance' _ [] _= Nothing
distance' (x:xs) (y:ys) num
    |x == y = distance' xs ys num
    |otherwise =  distance' xs ys (num + 1)