module Example where

import Data.Char

-- Problem: there are several ways to fix the program. No oracle given.
f1 xs = let g y = y : xs                         
        in g 1 ++ g True


----------------------------------------------
-- Explaining Type Errors by Finding the Source of Type Conflict

-- 7,14-14
