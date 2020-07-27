module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = traverse doOne

doOne :: Char -> Either Char Char
doOne x = case x of
    'G' -> Right 'C'
    'C' -> Right 'G'
    'T' -> Right 'A'
    'A' -> Right 'U'
    _ -> Left x

{-

import Control.Applicative (liftA2)
toRNA xs = foldr (liftA2 (:)) (pure []) $ map doOne xs

toRNA' xs = foldr (\acc x -> (:) <$> acc <*> x) (pure []) $ map doOne xs 

toRNA'' xs = foldr (\acc x -> fmap (:) acc <*> x) (pure []) $ map doOne xs

toRNA''' [] = pure []
toRNA''' (x:xs) = fmap (:) (doOne x) <*> toRNA xs

-}