module Phone (number) where
import Data.Char (isNumber)

number :: String -> Maybe String
number = c . d . f

f :: String -> String
f xs = filter isNumber xs

d :: String -> String
d xs = if head xs == '1' then drop 1 xs else xs

c :: String -> Maybe String
c xs = if length xs == 10 then n (Just xs) else Nothing

n :: Maybe String -> Maybe String
n (Just xs) = if (xs !! 0) `elem` i && (xs !! 3) `elem` i 
              then Just xs 
              else Nothing 
              where i = "23456789"