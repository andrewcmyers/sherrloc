module Example where

import Data.Char

-- Problem: the annotation should be [Char] -> Int
len :: [Int] -> Int                                   
len ls = len' 0 ls
    where len' n [] = n
          lengths n (_:xs) = len' (n+1) xs       
v81 = len "abc"          
-- 6,8-12
