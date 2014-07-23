module Example where

voegIn :: Int -> [Int] -> [Int]
voegIn a [] = a:[]
voegIn a (b:c) | a<=b = a:(b:c)
               | a>b = voegIn  a

-- 6,24-32
