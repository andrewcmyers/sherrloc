module Bouke where

slaplat :: [[Int]] -> [Int]
slaplat [] = []
slaplat n  = head n ++ slaplat (n-1)

-- (n-1) should be (tail n)
-- 5,32-36     5,34-34   5,35-35
