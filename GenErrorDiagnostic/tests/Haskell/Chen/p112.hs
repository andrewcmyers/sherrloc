module Example where

import Data.Char

-- Problem: there are several ways to fix the program. No oracle given.                
m f [] = []                                             
m f (x:xs) = f xs : m f x

-- 7,25-25
