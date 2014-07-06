module Example where

import Data.Char

-- Problem: ord should be something has argument type Bool
f x ys = ord (if x then x else null ys == x)                        

-- 6,10-12
