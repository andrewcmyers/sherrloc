module Example where

import Data.Char

-- Problem: should use \ x y -> ...
v3 = zipWith (\(x,y) -> x + y) [1,2] [3,4,5]          

-- 6,16-20
