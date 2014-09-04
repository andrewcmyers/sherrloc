module Abc where

abcFormule :: Float -> Float -> Float -> [Float]
abcFormule a b c= undefined 

aantalOpl :: Float -> Float -> Float -> Int






aantalOpl a b c = length (abcFormule a b c)

slaPlat :: [[Int]] -> [Int]
slaPlat [a] = a
slaPlat a = head a ++ (slaPlat (tail a))

voegIn :: Int -> [Int] -> [Int]
voegIn a b | null b = [a]
           | head b < a = a ++ b
           | otherwise = head b ++ (voegIn a (tail b))

-- ++ should be ':'
-- 21,29-30   22,33-34
