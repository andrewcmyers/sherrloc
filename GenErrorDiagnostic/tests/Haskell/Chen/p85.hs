module Example where

import Data.Char

-- Problem: 2 should be Bool or True should be Int
h = if True then (\f-> f (f 2))                                     
            else (\g-> g (g True))

-- 6,29-29  7,29-32
