module Example where

import Data.Char

-- Problem: True should be of type Char -> a
map' f [] = []                                          
map' f (x:xs) = f x ++ map' f xs
test = map True ['a','b','c']

-----------
-- yj.phd.pdf

-- 8,12-15
