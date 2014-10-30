module Example where

import Data.Char

-- Problem: [1 .. 5] and ((>10) . (^2)) should be swapped
v30 = map [1 .. 5] ((>10) . (^2))

-- 6,11-33   6,7-33
