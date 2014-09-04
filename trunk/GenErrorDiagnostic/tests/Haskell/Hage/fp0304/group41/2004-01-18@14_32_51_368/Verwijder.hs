module Verwijder where

verwijderRec :: (a-> a -> Bool) -> a -> [a] -> [a]
verwijderRec _ _ [] = []
verwijderRec func a (x:xs) | func a x = verwijderRec func a xs
                           | otherwise = x: verwijderRec func a xs


verwijderStFun :: (a -> a -> Bool) -> a -> [a] -> [a]
verwijderStFun func a lijst = filter (not.func a) lijst

groepeer :: Int -> [Int] -> [[Int]]
groepeer _ [] = []
groepeer a lijst = (take a lijst): groepeer (drop a lijst)

-- should be:  groepeer (drop a lijst)
-- 14,36-43  14,36-58 
