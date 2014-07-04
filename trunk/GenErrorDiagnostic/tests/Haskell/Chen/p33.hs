module Example where

import Data.Char

-- Problem: + should be ++
sumLists = sum2 . map sum2                       
sum2 [] = []
sum2 (x:xs) = x + sum2 xs

-----------------------------------------
-- Interactive type debugging in Haskell          

-- 8,17-17
