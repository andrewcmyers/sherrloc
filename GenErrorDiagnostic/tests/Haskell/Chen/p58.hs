module Example where

import Data.Char

-- Problem: (+) should be of type String -> String -> String
type Parser s a = [s ] -> [(a, [s ])]             
(<$>) :: (a -> b) -> Parser s a -> Parser s b
(<$>) = undefined

(<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
(<*>) = undefined

symbol :: Char -> Parser Char Char
symbol = undefined

token :: String -> Parser Char String
token = undefined

option :: Parser s a -> a -> Parser s a
option = undefined

maybeTwice = let p = map toUpper <$> token "hello"
             in option ((+) <$> p <*> p) []
-- 23,25-27  
