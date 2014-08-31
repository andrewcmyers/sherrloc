module Toets2 where

eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p = concat . map f
  where f x | eqBool (p x) True = [x]
            | otherwise         = []





data Boom a = Tak a (Boom a) (Boom a)
            | Blad


eqBoom :: (a -> a -> Bool) -> Boom a -> Boom a -> Bool
eqBoom cmp (Tak x y z) (Tak p q r) | x `cmp` y = eqBoom cmp y q && eqBoom cmp z r
                                   | otherwise = False
eqBoom cmp (Tak a b c) Blad  = False
eqBoom cmp Blad (Tak a b c)  = False
eqBoom cmp  Blad Blad = True

-- y should be: p
-- 20,46-46
