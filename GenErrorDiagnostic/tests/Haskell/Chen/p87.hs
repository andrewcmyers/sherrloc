module Example where

import Data.Char

-- Problem: 1 should be of type [Int]
f x = case x of                                                     
    0 -> [0]
    1 -> 1
fib x = case x of
    0 -> f x
    1 -> f x
    n -> head (f x)

------------
-- Discriminative Sum Types Locate the Source of Type Errors

-- Type safe in Haskell
