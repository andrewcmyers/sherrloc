module Toets where

verwijderRec :: (a -> a -> Bool) -> a -> [a] -> [a]
verwijderRec _ _ [] = []
verwijderRec v e (x:xs) | e `v` x =  verwijderRec v e xs
                        | otherwise = x : verwijderRec v e xs

verwijderStFun :: (a -> a -> Bool) -> a -> [a] -> [a]
verwijderStFun v e xs = filter (not.(e `v`)) xs

(%%) :: Int -> Int -> Bool
infixr 8 %%
x %% y = x rem y == 0

-- should be: x `rem` y
-- 13,10-16   13,10-10
