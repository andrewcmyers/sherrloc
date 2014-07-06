module Example where

import Data.Char

-- Problem: ++ should be +                               
f5 0 n = []
f5 m n = m ++ n : (f5 (m-1) n)

-- Type safe in Haskell
