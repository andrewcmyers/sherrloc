module Example where

import Data.Char

-- Problem: [] should be 0
sum' [] = []                                            
sum' (x:xs) = x `plus` sum' xs
plus = (+) :: Int -> Int -> Int

-- 6,11-12
