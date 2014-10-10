module Toets where

verwijderRec :: (a -> a -> Bool) -> a -> [a] -> [a]
verwijderRec f a [] = []
verwijderRec f a (x:xs) | f a x = verwijderRec f a xs
                        | otherwise = x : verwijderRec f a xs

verwijderStFun :: (a -> a -> Bool) -> a -> [a] -> [a]
verwijderStFun f a x = filter (not.f a) x

(%%) :: Int -> Int -> Bool
infix 7 %%
x %% y | (x `rem` y == 0) = True
       | otherwise = False

groepeer :: Int -> [Int] -> [[Int]]
groepeer _ [] = []
groepeer n x = take n x : groepeer n (drop n x)








data Boom a = Tak a (Boom a) (Boom a)
            | Blad


eqBoom :: (a -> a -> Bool) -> Boom a -> Boom a -> Bool
eqBoom f (Tak a1 a2 a3) (Tak b1 b2 b3) = if (f a1 b1) then True && (eqBoom f a2 b2) && (eqBoom f a3 b3) else False
eqBoom f Blad Blad = True
eqBoom f _ _ = False

overal :: (a -> b) -> Boom a -> Boom b
overal f (Tak a1 a2 a3) = (Tak (f a1) (overal a2) (overal a3))
overal f Blad = Blad

-- should be: (overal f a2) (overal f a3)
-- 37,47-48   37,40-48   37,59-60   37,52-60
