{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Example where

import Data.Char

-- Problem: len or 10 needs an annotation
class Mul a b c | a b -> c where
    mul :: a -> b -> c
    
instance Mul Int Int Int where
    mul = (*)

f xs = let len = length xs
       in len `mul` 10
zero = f []

-- 15,11-13  15,21-22
