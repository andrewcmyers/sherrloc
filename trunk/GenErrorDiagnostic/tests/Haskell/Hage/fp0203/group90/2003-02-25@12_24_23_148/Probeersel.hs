module Probeersel where

eqString      :: String -> String -> Bool 
eqString = undefined

fac :: Int -> Int
fac n = product [1..n]

abcFormule :: Float   -> Float -> Float -> [Float]
abcFormule a b c = undefined 


aantalOpl :: Float -> Float -> Float -> Float
aantalOpl a b c = undefined 

slaPlat :: [[Int]] -> [Int]
slaPlat  []    = []
slaPlat (x:xs) = x ++ slaPlat xs

voegIn :: Int -> [Int] -> [Int]
voegIn a []       = a:[]
voegIn a (x:xs) | a<x = a:(x:xs)
                |otherwise = x: (voegIn a xs)

selecteer :: [Int] -> [[Int]] -> [Int]
selecteer [] [[]] = []
selecteer [] (ys:yss) = slaPlat (ys:yss)
selecteer (x:xs) [] = (x:xs)
selecteer (x:xs) (ys:yss) = reverse(dropWhile (notEqInt (last (x:xs)))(reverse(dropWhile (notEqInt x) (slaPlat (ys:yss)))))

naEerste :: Int -> [Int] -> [Int]
naEerste a [] = drop a []
naEerste a (x:xs) = dropWhile (notEqInt a) (x:xs)

notEqInt :: Int -> Int -> Bool
notEqInt a b | b==a = False
             | a/=b = True


notEqString :: String -> String -> Bool
notEqString "a" "b" | eqString "b" "a" = False
                    | otherwise = True

selecteer2 :: [String] -> [[String]] -> [String]
selecteer2 [] [[]] = []
selecteer2 [] (ys:yss) = slaPlat (ys:yss)
selecteer2 (x:xs) [] = (x:xs)
selecteer2 (x:xs) (ys:yss) = reverse(dropWhile (notEqString (last (x:xs)))(reverse(dropWhile (notEqString x) (slaPlat2 (ys:yss)))))

slaPlat2 :: [[String]] -> [String]
slaPlat2  []    = []
slaPlat2 (x:xs) = x ++ slaPlat2 xs

-- slaPlat should be: slaPlat2
-- 46,26-32
