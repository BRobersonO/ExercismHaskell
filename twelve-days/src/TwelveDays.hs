module TwelveDays (recite) where

data Days = 
    D1
    |D2
    |D3
    |D4
    |D5
    |D6
    |D7
    |D8
    |D9
    |D10
    |D11
    |D12
    deriving (Enum, Eq)
   
instance Show Days where
    show D1 = "a Partridge in a Pear Tree."
    show D2 = "two Turtle Doves, and " ++ show D1
    show D3 = "three French Hens, " ++ show D2
    show D4 = "four Calling Birds, " ++ show D3
    show D5 = "five Gold Rings, " ++ show D4
    show D6 = "six Geese-a-Laying, " ++ show D5
    show D7 = "seven Swans-a-Swimming, " ++ show D6
    show D8 = "eight Maids-a-Milking, " ++ show D7
    show D9 = "nine Ladies Dancing, " ++ show D8
    show D10 = "ten Lords-a-Leaping, " ++ show D9
    show D11 = "eleven Pipers Piping, " ++ show D10
    show D12 = "twelve Drummers Drumming, " ++ show D11
    
recite :: Int -> Int -> [String]
recite start stop     
    |start == stop = [whichDay start ++ (show (toEnum (stop - 1)::Days))]
    |otherwise =  (whichDay start ++ show (toEnum (start - 1)::Days)) : recite (start + 1) stop
    
whichDay :: (Eq a, Num a) => a -> [Char]
whichDay x = "On the " ++ switch x ++ "day of Christmas my true love gave to me: "  where
    switch i = case i of 
        1 -> "first "
        2 -> "second "
        3 -> "third "
        4 -> "fourth "
        5 -> "fifth "
        6 -> "sixth "
        7 -> "seventh "
        8 -> "eighth "
        9 -> "ninth "
        10 -> "tenth "
        11 -> "eleventh "
        12 -> "twelfth "
