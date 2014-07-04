module Example where

import Data.Char

-- Problem: last u should be of type a where a is the argument type of x 
v66 = \x -> let f = \y -> y x                                 
            in f (\z -> z) (f (\u -> u))
      
-- 7,28-40
