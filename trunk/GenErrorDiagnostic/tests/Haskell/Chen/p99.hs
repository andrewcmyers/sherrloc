module Example where

import Data.Char

-- Problem: the variable t has type [a] should be of type a
m f (h:t) = f h : [f t]                                   

-- 6,22-22
