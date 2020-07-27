module ResistorColors (Color(..), Resistor(..), label, ohms) where
import Data.List (isSuffixOf)

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
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

label :: Resistor -> String
label resistor
    |ohms resistor > 999999999 = rounder ((fromIntegral (ohms resistor)) / 1e9) ++ " gigaohms"
    |ohms resistor > 999999 = rounder ((fromIntegral (ohms resistor)) / 1e6) ++ " megaohms"
    |ohms resistor > 999 = rounder ((fromIntegral (ohms resistor)) / 1e3) ++ " kiloohms"
    |otherwise = show (ohms resistor) ++ " ohms"

ohms :: Resistor -> Int
ohms (Resistor (x, y, z)) = (10 * fromEnum x + fromEnum y) * 10 ^ fromEnum z

rounder :: (Show a, RealFrac a) => a -> String
rounder x
    |isSuffixOf ".0" (show x) = show $ round x
    |otherwise = show $ x
