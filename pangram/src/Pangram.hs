module Pangram (isPangram) where
import Data.List (intersect, nub, sort)
import Data.Char (toLower, isAlpha, isAscii)

isPangram :: String -> Bool
isPangram text = 
    ['a'..'z'] == (nub.sort) (map toLower $ filter isAlpha $ filter isAscii text)
{-
--This also works:
isPangram text = 
    ['a'..'z'] == (nub (sort (intersect ['a'..'z'] (map toLower text))))
-}

{-
--Why does this not work?:
isPangram text = 
    ['a'..'z'] == (nub.sort) (map toLower (filter (isAlpha && isAscii) text))
-}