module Example where

import Data.Char

-- Problem: "" and token "hello" should be swapped
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


test13 :: Parser Char String                        
test13 = option "" (token "hello!")

-- 24,17-35    24,10-35
