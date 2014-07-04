module Example where

import Data.Char

-- Problem: foldr (+) xs should be foldr (+) 0 xs
f = \xs -> (length xs, foldr (+) xs + 0)      

-- 6,34-35
