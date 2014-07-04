module Example where

import Data.Char

-- Problem: there are several ways to fix the program. No oracle given.            
v24 = \x -> let g = \y -> (y : x) in g 1 ++ g True         

-- 6,40-40  6,47-50
