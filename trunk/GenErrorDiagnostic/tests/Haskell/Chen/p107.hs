module Example where

import Data.Char

-- Problem: [0.0] should be 0.0
f 1 x = [0.0]                                          
f n x = n*x + f (n-1) x

-- Type safe in Haskell
