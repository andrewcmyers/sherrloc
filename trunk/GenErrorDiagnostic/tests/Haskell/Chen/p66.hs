module Example where

import Data.Char

-- Problem: True should be something of type Char -> a
map1 f [] = []                                                       
map1 f (x:xs) = f x : map1 f xs
test = map1 True ['a','b','c']

-- 8,13-16 
