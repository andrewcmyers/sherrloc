module ParserLibrary where


type Parser a = String -> [(a, String)]

eqString      :: String -> String -> Bool 
eqString = undefined
eqChar      :: Char -> Char -> Bool
eqChar = undefined
isAlpha :: Char -> Bool
isAlpha = undefined

letter:: Char -> Parser Char
letter _ [] = []
letter a (x:xs) |  eqChar x a   = [(a,xs)]
                |  otherwise = []

tok:: String -> Parser String
tok k xs |eqString k (take n xs) = [(k, drop n xs)]
         |otherwise     = []
                    where n = length k

sequ:: Parser a -> Parser b -> Parser (a,b)
(p1 `sequ` p2) xs = [ ((v1,v2),xs2) | (v1,xs1) <- p1 xs , (v2, xs2) <- p2 xs1]

orelse:: Parser a -> Parser a -> Parser a
(p1 `orelse` p2) xs = p1 xs ++ p2 xs

just:: Parser a -> Parser a
just p xs = [ (v, "") | (v,ys) <- p xs, eqString ys ""]

doe:: Parser  a -> (a-> b) -> Parser b
doe p f xs = [(f v , ys) | (v,ys) <- p xs]

infixr 5 `sequ`
infixr 3 `orelse`
infix 7 `doe`

haakjes  = ( letter '('
           `sequ` haakjes
           `sequ` letter ')'
           `sequ` haakjes
           )
           `doe` (\(a,(b,(c,d))) -> (1+b) `max` d)
           `orelse` tok "" `doe` \x -> 0

variabele:: Parser String
variabele xs = [(takeWhile isAlpha xs, dropWhile isAlpha xs)]

bool::  Parser Bool
bool xs | eqString "False" (take 5 xs) = [(False, drop 5 xs)]
        | eqString "True"  (take 4 xs) = [(True, drop 4 xs)]
        | otherwise = []
