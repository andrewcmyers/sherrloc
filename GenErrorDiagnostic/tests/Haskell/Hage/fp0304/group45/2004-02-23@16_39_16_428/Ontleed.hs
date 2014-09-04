module Ontleed where

import ParserLibrary





haakje_openen  :: Parser Char [Char]
haakje_sluiten :: Parser Char [Char]

haakje_openen xs = token  eqChar "(" xs
haakje_sluiten xs = token eqChar ")" xs


token_en :: Parser Char [Char]
token_en xs  = token eqChar "/\\"  xs
token_of :: Parser Char [Char]
token_of xs = token eqChar "\\/" xs

whitespace:: Parser Char [Char]
whitespace xs = satisfy isSpace  xs


-- signature should be: Parser Char Char
-- 21,14-31
