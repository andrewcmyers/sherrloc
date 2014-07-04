module Example where

import Data.Char

-- Problem: first not should be of type Int -> Int, second not should
-- be of type Bool -> Int
ff x = if True then not x else x + 1                                 
gg x = if True then not x else 2
v55 = ff 3 + gg True

-- 7,21-25  8,21-25
