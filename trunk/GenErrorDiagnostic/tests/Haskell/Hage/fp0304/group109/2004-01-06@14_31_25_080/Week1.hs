module Week1 where

voegIn :: Int -> [Int] -> [Int]
voegIn a [] = [a]
voegIn a (x:xs) | a<= x = a:x:xs
                | a> x = x: (voegIn a xs)

insertionSort:: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = voegIn x (insertionSort xs)




























vanafLaatste :: [Int] -> [Int]
vanafLaatste (x:xs) | x /= 0 = vanafLaatste xs
                    | x == 0 && (laatste0 x) = xs
                    | x == 0 && not (laatste0 x) = vanafLaatste xs

laatste0 :: [Int] -> Bool
laatste0 [] = True
laatste0 (x:xs) | x /= 0 = laatste0 xs
                | x == 0 = False



alles [] a b = []
alles (x:xs) a b | x /= 0 = a
                 | x == 0 = b


totEerste (x:xs) = alles (x:xs) (x:(totEerste xs)) []


-- x should be xs
-- 41,43-43   42,47-47
