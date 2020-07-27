module Yacht (yacht, Category(..)) where

import Data.List

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

yacht :: Category -> [Int] -> Int
yacht category dice = case category of 
    Ones -> sum [x | x <- dice, x == 1]
    Twos -> sum [x | x <- dice, x == 2]
    Threes -> sum [x | x <- dice, x == 3]
    Fours -> sum [x | x <- dice, x == 4] 
    Fives -> sum [x | x <- dice, x == 5] 
    Sixes -> sum [x | x <- dice, x == 6] 
    FullHouse -> if check dice then sum dice else 0
    FourOfAKind -> if f dice == 1 then sum (take 4 (reverse (sort dice))) 
        else if f dice == 4 then sum (take 4 (sort dice)) 
        else if all (== (head dice)) dice then sum (take 4 dice)
        else 0
    LittleStraight -> if sort dice == [1,2,3,4,5] then 30 else 0
    BigStraight -> if sort dice == [2,3,4,5,6] then 30 else 0
    Choice -> sum dice
    Yacht -> if all (== (head dice)) dice then 50 else 0

check :: [Int] -> Bool
check xs =
    (f xs == 2 && g xs == 3) || 
    (f xs == 3 && g xs == 2)
    
f = length.head.group.sort
g = length.head.reverse.group.sort