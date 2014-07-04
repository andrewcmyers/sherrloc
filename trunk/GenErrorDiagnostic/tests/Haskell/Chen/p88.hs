module Example where

import Data.Char

-- Problem: there are several ways to fix the program. No oracle given.                
v56 = (\f -> f 1) (\y -> if y then 1 else 0)                           

----------
-- Proofs about a Folklore Let-Polymorphic Type Inference Algorithm

-- 6,16-16
