module Example where

import Data.Char

-- Problem: w should be even w, True should be of type Int        
v19 = \x -> let w = x +1                              
            in if w then [1,True] else [4]

----------------------------------------
-- Improving Type Error Diagnosis
-- (Not including examples regarding type annotations and type classes)

-- 7,19-19
