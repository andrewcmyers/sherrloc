module Example where

import Data.Char

-- Problem: there are several ways to fix the program. No oracle given.            
v14 = (\x -> x + (x 4)) (if True then True else 1)    

-- 6,14-14   6,16-16   6,39-42  6,49-49
