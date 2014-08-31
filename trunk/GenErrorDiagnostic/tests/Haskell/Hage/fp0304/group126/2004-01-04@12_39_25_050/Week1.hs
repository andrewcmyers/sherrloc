module Week1 where

aantalOpl :: Float -> Float -> Float -> Int
aantalOpl = undefined
 
slaPlat :: [[a]] -> [a]
slaPlat []        = []
slaPlat (x:xs)    = x ++ slaPlat xs

voegIn :: Int -> [Int] -> [Int]
voegIn a []              = [a]
voegIn a (x:xs) | a <= x = a:(x:xs)
                | a > x  = x:(voegIn a xs)

insertionSort :: [Int] -> [Int]
insertionSort []     = []
insertionSort (x:xs) = voegIn x (insertionSort xs)

totEerste :: [Int] -> [Int]
totEerste []     = []
totEerste (0:_)  = []
totEerste (x:xs) = x:totEerste xs

naEerste :: [Int] -> [Int]
naEerste []     = []
naEerste (0:xs) = xs
naEerste (_:xs) = naEerste xs

zonder :: [Int] -> [Int]
zonder []     = []
zonder (0:xs) = zonder xs
zonder (x:xs) = x:zonder xs

vanafLaatste :: [Int] -> [Int]
vanafLaatste l = reverse (totEerste (reverse l))

slaPlat1 :: [[a]] -> [a]
slaPlat1 l = foldr ++ [] l

-- missing () around ++
-- 38,20-21
