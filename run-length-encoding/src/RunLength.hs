module RunLength (decode, encode) where

import Data.List
import Data.Char

decode :: String -> String
decode [] = []
decode (x:y:z:xs)
    |isNumber x && isNumber y = replicate ((digitToInt y) + ((digitToInt x) * 10)) z ++ decode xs
    |isNumber x = replicate (digitToInt x) y ++ decode (z:xs)
    |otherwise = x:decode (y:z:xs)
decode (x:y:xs)
    |isNumber x = replicate (digitToInt x) y ++ decode (xs)
    |otherwise = x:decode (y:xs)
decode (x:xs) = (x:xs)
    
encode :: String -> String
encode text = concat( map f (group text))

f [] = []
f [x] = [x]
f (x:xs) = show ((length xs) + 1) ++ [x]