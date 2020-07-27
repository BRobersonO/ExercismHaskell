module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
    |n < 1 || n > 64 = Nothing
    |otherwise = Just (square' n 1)
    
square' 1 acc = acc
square' n acc = square' (n - 1) (acc * 2)

total :: Integer
total = sum $ map (\x -> square' x 1) [1..64]