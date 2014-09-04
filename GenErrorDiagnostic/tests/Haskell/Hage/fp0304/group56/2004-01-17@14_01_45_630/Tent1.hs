module Tent1 where

             (%%) :: Int -> Int -> Bool
             a %% b = (mod a b ==0)

             verwijderRec :: (a -> a -> Bool) -> a -> [a] -> [a]
             verwijderRec _ _ [] = []
             verwijderRec q r (x:xs) | (q r) = verwijderRec q r xs
                                     | otherwise = x:verwijderRec q r xs

-- (q r) should be: (q r x)
-- 8,41-41   8,41-43
