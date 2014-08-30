module Stom where
slaPlat :: [[Int]] -> [Int]

slaPlat [] = []
slaPlat (x:xs) = x ++ (slaPlat xs)

voegIn :: Int -> [Int] -> [Int]

voegIn x [] = [x]
voegIn x (y:ys)  | x < y  = (x:y:ys)
                 | otherwise  = y : voegIn x ys

insertionSort :: [Int] -> [Int]

insertionSort [] = []
insertionSort (x:xs) =  voegIn x (insertionSort xs)

totEerste :: [Int] -> [Int]

totEerste [] = []
totEerste (x:xs) | x > 0 = x : (totEerste xs)
                 | x == 0 = []

naEerste :: [Int] -> [Int]

naEerste [] = []
naEerste (x:xs) | x > 0 = naEerste xs
                | x == 0 = [xs]

-- [xs] should be xs
-- 28,28-31   28,29-30
