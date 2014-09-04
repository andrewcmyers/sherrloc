module Prakticum1 where

slaPlat :: [[a]] -> [a]

slaPlat []     = []
slaPlat (x:xs) = x ++ (slaPlat xs)




voegIn :: Int -> [Int] -> [Int]

voegIn z [] = [z]
voegIn z (x:xs) |(z <= x) = [z] ++ (x:xs)
                |True     = x ++ (voegIn z xs)



insertionSort :: [Int] -> [Int]
insertionSort []     = []
insertionSort (x:xs) = voegIn x (insertionSort xs)

-- x should be [x]
-- 15,29-29
