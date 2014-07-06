module Example where

import Data.Char

-- Problem: == should of type Int -> Int -> Int
fac n = if n == 0 then 1 else n * fac (n == 1)          

-- 6,39-46
