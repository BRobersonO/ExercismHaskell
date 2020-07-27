module IsbnVerifier (isbn) where
import Data.Char (isNumber)

isbn :: String -> Bool
isbn xs = if and [check0 xs, check1 xs, check2 xs] 
    then checkX xs 
    else False

--Bools to check
check0 :: Foldable t => t a -> Bool
check0 xs = not $ null xs

check1 :: [Char] -> Bool
check1 xs = if containsX (noDashes xs) 
    then length (onlyNums xs) == 9 
    else length (onlyNums xs) == 10

check2 :: [Char] -> Bool
check2 xs = and $ map (\x -> x `elem` "0123456789-X") xs
    
checkX :: [Char] -> Bool
checkX xs = (sum (zipWith (*) ((listNums xs) ++ [10]) [10,9..])) `mod` 11 == 0

--Other Helpers
noDashes :: [Char] -> [Char]
noDashes = filter (\x -> x /= '-')

containsX :: [Char] -> Bool
containsX xs = last xs == 'X'

listNums :: [Char] -> [Int]
listNums xs = map (read . (:"")) (onlyNums xs) :: [Int]

onlyNums :: [Char] -> [Char]
onlyNums = filter isNumber