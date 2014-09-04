module Text where

slaPlat :: [[Int]] -> [Int]

slaPlat [] = []
slaPlat(kop:staart) = kop ++ slaPlat(staart)



voegIn :: Int -> [Int] -> [Int]

voegIn x [] = [x]

voegIn x (kop:staart) | kop>=x = x ++ kop ++ staart
                      | kop<x = voegIn x staart

-- ++ should be ':'
-- 14,36-37   14,43-44
