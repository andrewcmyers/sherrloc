module Example where

import Data.Char

-- Problem: there are several ways to fix the program. No oracle given.            
f g x y = (g (if x then x else y), g "abc")

-- 6,25-25  6,38-42
