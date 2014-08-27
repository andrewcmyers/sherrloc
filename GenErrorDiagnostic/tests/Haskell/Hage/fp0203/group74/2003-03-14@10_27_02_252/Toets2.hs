module Toets2(module List, module Toets2) where

import Data.List

data Boom a = Tak a (Boom a) (Boom a)
            | Blad






eqBoom :: (a -> a -> Bool) -> Boom a -> Boom a -> Bool
eqBoom cmp Blad Blad = True
eqBoom cmp (Tak x li1 re1) (Tak y li2 re2) | x `cmp` y = eqBoom cmp li1 li2 && eqBoom cmp re1 re2
                                           | otherwise = False

overal :: (a -> b) -> Boom a -> Boom b
overal _ Blad = Blad
overal f (Tak x li re) = Tak (f x) (overal li) (overal re)

-- should be: overal f li and overal f re
-- 20,37-45   20,49-57
