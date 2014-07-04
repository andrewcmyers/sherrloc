module Example where

import Data.Char

-- Problem: biggest should of type Int, rather than [Int] -> Int
normalise xs = scale biggest xs                                   
scale x ns = map (/x) ns
biggest (x:xs) = max x xs
            where max x [] = x
                  max x (y:ys) | y > x = max y ys

-- Type safe in Haskell
