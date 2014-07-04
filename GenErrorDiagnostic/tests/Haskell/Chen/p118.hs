module Example where

import Data.Char

-- Problem: there are several ways to fix the program. No oracle given.                
v80 = \f g a -> (f a, f 1, g a, g True)

--------------------
-- Typehope: There is hope for your type erros              

-- 6,25-25 6,35-38
