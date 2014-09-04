module Toets where

verwijderRec :: (a->a->Bool)->a->[a]->[a]
verwijderRec test x [] = []
verwijderRec test x (k:staart) | test x k = verwijderRec test x staart
                               | otherwise = k:verwijderRec test x staart

verwijderStFun :: (a->a->Bool)->a->[a]->[a]
verwijderStFun test x lijst = filter (not.test x) lijst







(%%)::Int->Int->Bool
(%%) a b | a `rem` b == 0 = True
         | otherwise = False

groepeer :: Int->[Int]->[[Int]]
groepeer x [] = []
groepeer x lijst = (take x lijst):(groepeer x (drop x lijst))

remove :: [a]->[a]->[a]
remove x [] = []
remove (x:xs) (y:ys) | x == y = remove (x:xs) ys
                     | otherwise = x:remove xs ys

-- signature should be: [Int] -> [Int] -> [Int]
-- 25,11-23   25,1-7
