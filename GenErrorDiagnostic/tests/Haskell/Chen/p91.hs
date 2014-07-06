module Example where

import Data.Char

-- Problem: second == should have type Int -> Int -> Int
fac n = if n ==0 then 1 else n * fac (n==1)             

-----------
-- type error reporting in the hindley milner system

-- 6,39-42
