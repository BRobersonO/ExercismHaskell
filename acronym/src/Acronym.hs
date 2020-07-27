module Acronym (abbreviate) where
import Data.Char

abbreviate :: String -> String
abbreviate [] = []
abbreviate [_] = []
abbreviate (x:xs) = toUpper x : abbreviate' xs
abbreviate' :: [Char] -> [Char]
abbreviate' [] = []
abbreviate' [_] = []
abbreviate' (x:y:xs)
    | x `elem` ['a'..'z'] && y `elem` ['A'..'Z'] = toUpper y : abbreviate' (xs)
    | x == ' ' && y `elem` ['A'..'z'] = toUpper y : abbreviate' xs
    | x == '-' && y `elem` ['A'..'z'] = toUpper y : abbreviate' xs
    | otherwise = abbreviate' (y:xs)