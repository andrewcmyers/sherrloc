module Example where

import Data.Char

-- Problem: (-1) should be ((-) 1)
decrementList :: [Int ] -> [Int ]                 
decrementList xs = map (-1) xs

-- 7,24-27
