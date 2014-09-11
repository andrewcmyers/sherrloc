module Toets1 where

verwijderRec :: (Int -> Int -> Bool) -> Int -> [Int] -> [Int]
verwijderRec test n []     = []
verwijderRec test n (x:xs) | x==n = verwijderRec test n xs
                           | x/=n = x:verwijderRec test n xs

verwijderStFun :: (Int -> Int -> Bool) -> Int -> [Int] -> [Int]
verwijderStFun test n [] = []
verwijderStFun test n (x:xs) = filter (n/=) (x:xs)

groepeer :: Int -> [Int] -> [[Int]]
groepeer n [] = []
groepeer n (x:xs) = (x:take (n-1) xs)

-- signature should be: Int -> [Int] -> [Int]
-- 12,13-35   12,29-35   12,1-8
