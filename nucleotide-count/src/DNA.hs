module DNA (nucleotideCounts, Nucleotide(..)) where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foldM f initialMap xs
    
f acc x = (M.adjust (+1)) <$> transform x <*> (pure acc)

initialMap = M.fromList [(A, 0)
                        ,(C, 0)
                        ,(G, 0)
                        ,(T, 0)]
                        
transform x = case x of 
    'A' -> Right A
    'C' -> Right C
    'G' -> Right G
    'T' -> Right T
    _ -> Left "Invalid"