module Toets where

filteren :: (a -> Bool) -> [a] -> [b]
filteren p = concat . map f
   where f x  | p x       = [x]
              | otherwise = []

-- signature should be: (a -> Bool) -> [a] -> [a]
-- 3,13-37   3,1-9
