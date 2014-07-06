module Example where

import Data.Char

-- Problem: <* should be <*> (or something has the same type as
-- <*>)
-- Note the following example resembles the example in Figure 9.1
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

test14 = (++) <$> ( (:) <$> symbol '!'
                        <* token "hello")
              <$> token "world"
-- Parsing error
