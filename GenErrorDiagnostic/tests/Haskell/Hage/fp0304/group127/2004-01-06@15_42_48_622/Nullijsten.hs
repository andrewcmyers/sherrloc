module Nullijsten where

totEerste :: [Int] -> [Int]
totEerste [] = []
totEerste (x:xs) | x==0 = x
                 |otherwise = x: totEerste (xs)

-- x should be: [x]
-- 5,27-27
