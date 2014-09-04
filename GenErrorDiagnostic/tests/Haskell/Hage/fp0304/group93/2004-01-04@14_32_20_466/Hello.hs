module Hello where

hello :: Int -> [Char]
hello n = concat (replicate n "hello")

aantal0p1 :: Int -> Int -> Int -> Int
aantal0p1 a b c | d < 0 = 0
                | d == 0 = 1
                | d > 0 = 2
                where d = (b*b-4*a*c)








evenen :: Int -> Bool
evenen n | rem n 2 == 0 = True
         | otherwise = False

slaPlat :: [[Int]] -> [Int]
slaPlat [x] = x
slaPlat (x:xs) = x ++ slaPlat xs

voegIn :: Int -> [Int] -> [Int]
voegIn x [] = [x]
voegIn x (b:bs) | (x >= b) = (x : (b:bs))
                | (x < b) = (b : (voegIn x bs))

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = voegIn x (insertionSort xs)

totEerste :: [Int] -> [Int]
totEerste [] = []
totEerste (x:xs) | (x /= 0) = (x : (totEerste xs))
                 | (x == 0) = []

naEerste :: [Int] -> [Int]
naEerste [] = []
naEerste (x:xs) | (x == 0) = xs
                | (x /= 0) = naEerste xs

zonder :: [Int] -> [Int]
zonder [] = []
zonder (x:xs) | (x == 0) = zonder xs
              | (x /= 0) = (x : (zonder xs))

vanafLaatste :: [Int] -> [Int]
vanafLaatste x = totEerste reverse x

-- missing () around reverse x
-- 52,18-26   52,18-36
