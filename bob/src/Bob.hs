{-# LANGUAGE OverloadedStrings #-}
module Bob (responseFor) where
import qualified Data.Text as T 
import Data.Char (isUpper, isAlpha, isSpace)
import Data.Text (Text)

responseFor :: Text -> Text
responseFor xs
    |T.all isSpace xs = "Fine. Be that way!"
    |T.all isUpper (T.filter isAlpha xs) 
        && T.any isAlpha xs 
        && T.isSuffixOf "?" (T.dropWhileEnd isSpace xs) 
            = "Calm down, I know what I'm doing!"
    |T.all isUpper (T.filter isAlpha xs) 
        && T.any isAlpha xs 
            = "Whoa, chill out!"
    |T.isSuffixOf "?" (T.dropWhileEnd isSpace xs) = "Sure."
    |otherwise = "Whatever."