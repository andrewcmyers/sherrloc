module Opdr4 where

naEerste :: [Int] -> [Int]

naEerste (x:y) | x >  0 = naEerste [y]
               | x == 0 = [y]

-- [y] should be: y
-- 5,36-38  5,37-37
