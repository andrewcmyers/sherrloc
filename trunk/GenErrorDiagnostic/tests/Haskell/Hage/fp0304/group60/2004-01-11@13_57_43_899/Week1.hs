module Week1 where

aantalOpl :: Float -> Float -> Float -> Int
aantalOpl a b c | dis > 0.0 =2
                | dis < 0.0 =0
                | dis == 0.0 = 1
                  where dis = (b^2 - 4.0*a*c)

slaPlat :: [[a]] -> [a]
slaPlat [] = []
slaPlat (h:t) = h++slaPlat t

voegIn :: Int -> [Int] -> [Int]
voegIn a [] = [a]
voegIn a (b:t) | a <= b = a:(b:t)
               | a > b = b:(voegIn a t)

insertionSort  :: [Int] -> [Int]
insertionSort [] = []
insertionSort (h:t) = voegIn h (insertionSort t)

totEerste :: [Int]->[Int]
totEerste [] = []
totEerste (a:t) |a==0 =[]
                |a/=0 = a:(totEerste t)

naEerste :: [Int] -> [Int]
naEerste [] = []
naEerste (a:t) | a == 0 = t
               | a /= 0 = naEerste t

zonder :: [Int] -> [Int]
zonder[] = []
zonder(a:t) |a==0 = zonder t
            |a/=0 = a:(zonder t)






x :: Int
x = 5.0

-- 5.0 should be: 5
-- 43,5-7
