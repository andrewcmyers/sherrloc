module Example where

import Data.Char

-- Problem: [0] should be 0
f x = case x of                                                     
    0 -> [0]
    1 -> 1
plus :: Int -> Int -> Int
plus = (+)
fib x = case x of
    0 -> f x
    1 -> f x
    n -> fib (n-1) `plus` fib (n-2)

-- 7,10-12
