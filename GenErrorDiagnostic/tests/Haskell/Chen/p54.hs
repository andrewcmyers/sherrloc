module Example where

import Data.Char

-- Problem: (x,xs) should be (x:xs)                   
maxOfList [ ] = error "empty list"
maxOfList [x] = x
maxOfList (x , xs) = x `max` maxOfList xs


-- 8,11-18
