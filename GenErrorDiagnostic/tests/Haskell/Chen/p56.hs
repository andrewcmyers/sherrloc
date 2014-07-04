module Example where

import Data.Char

-- Problem: type of (++) doesn't match String -> Char -> String
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


test12 :: Parser Char String                        
test12 = (++) <$> token "hello world"
              <*> symbol '!'

-- 24,10-13
