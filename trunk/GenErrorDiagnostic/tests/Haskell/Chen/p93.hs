module Example where

import Data.Char

-- Problem: 3 should be of type Bool
v60 = \f y x -> f (y x) (y 3) (not x)                         

-- 6,28-28
