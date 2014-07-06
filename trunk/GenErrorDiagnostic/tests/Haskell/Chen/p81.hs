module Example where

import Data.Char

-- Problem: False should be something of Int
v50 = (\x->x+3) (if True then False else 1)                               

-- 6,31-35
