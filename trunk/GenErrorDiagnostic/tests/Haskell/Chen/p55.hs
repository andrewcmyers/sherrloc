module Example where

import Data.Char

-- Problem: right of <$> is not a Parser type
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


test11 :: Parser Char String                        
test11 = map toUpper <$> "hello, world!"

-- 24,26-40
