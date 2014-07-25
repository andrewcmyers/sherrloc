module Practicum where
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
kolomBr :: [String] -> Int
kolomBr (x:xs) |eqList eqString xs [] =  length x
               |length x < length (head xs) = kolomBr xs
               |otherwise = kolomBr (x:(drop 1 xs))
kolomBr [] = 0
type Table = [[String]]
neemKolom :: Table -> Int -> [String]
neemKolom (x:xs) k = x!!k-1 : neemKolom xs k
neemKolom [] _= []
-- k-1 should be (k-1)
-- 13,25-27 
