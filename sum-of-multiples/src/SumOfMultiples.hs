module SumOfMultiples (sumOfMultiples) where

import qualified Data.Set as S

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
    sum $ S.fromList [x | f <- factors
                     , f /= 0
                     , x <- [f, (f*2)..(limit-1)]
                     ]