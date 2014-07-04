module Example where

import Data.Char

-- Problem: xs and 0 should be swapped
sumFloat :: [Float ] -> Float                     
sumFloat xs = foldr (+) xs 0 + 0.0

-- 7,25-28
