module Example where

import Data.Char

-- Problem: v should be of type Bool
f [] = False                                            
f [v] = v
f (h1:h2:t) = h1 `gt` h2
gt :: Int -> Int -> Bool
gt = undefined

-------------------
-- Unification source-tracking with application to diagnosis of type inference

-- 7,9-9
