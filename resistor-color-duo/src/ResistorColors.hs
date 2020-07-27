module ResistorColors (Color(..), value) where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show)

value :: (Color, Color) -> Int
value (a, b) = 10 * toDigit a + toDigit b
    where toDigit x 
            | x == Black = 0
            | x == Brown = 1
            | x == Red = 2
            | x == Orange = 3
            | x == Yellow = 4
            | x == Green = 5
            | x == Blue = 6
            | x == Violet = 7
            | x == Grey = 8
            | x == White = 9
