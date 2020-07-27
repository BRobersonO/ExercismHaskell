module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify x 
    |x < 1 = Nothing
    |c == x = Just Perfect
    |c > x = Just Abundant
    |c < x = Just Deficient
    |otherwise = error "invalid"
    where c = sum [i | i <- [1..x], x `mod` i == 0, i /= x]