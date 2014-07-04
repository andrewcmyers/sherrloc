module Example where

import Data.Char

-- Problem: [xs] should be xs
merge [] ys = ys                                                    
merge xs [] = [xs]
merge (x:xs) (y:ys) | x < y = [x] ++ merge xs (y:ys)
                    | otherwise = [y] ++ merge (x:xs) ys

-- 7,15-18
