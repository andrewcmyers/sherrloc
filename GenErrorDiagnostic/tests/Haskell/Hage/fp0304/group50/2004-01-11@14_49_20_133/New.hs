module New where

f :: a -> a

f x = [0..x]

-- signature should be: Int -> [Int]
-- 3,1-1   3,6-11    3,6-6    3,11-11
