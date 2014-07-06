module Example where

import Data.Char

-- Problem: there are several ways to fix the program. No oracle given.                
f (c:cs) (i:is) = if i > 0 then f cs is                
                           else f is (c:[2.2])

-------------------
-- Helping students understand polymorphic type errors

-- Type safe in Haskell
