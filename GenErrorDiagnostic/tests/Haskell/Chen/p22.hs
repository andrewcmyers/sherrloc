module Example where

import Data.Char

-- Problem: True should be Int
v13 = (\x -> x + 4) (if True then True else 1)        

-- 6,35-38
