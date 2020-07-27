module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz x = if x < 1 then Nothing else tailRecCol x 0 where
    tailRecCol i acc
        |i == 1 = Just acc
        |even i = tailRecCol (i `div` 2) $ acc + 1
        |otherwise = tailRecCol (3 * i + 1) $ acc + 1