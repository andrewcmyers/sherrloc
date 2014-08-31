module Zomaar where

eqChar      :: Char -> Char -> Bool
eqChar = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

plus :: Int -> Int -> Int
plus x y = x + y

w :: [[Char]] -> String
w [] = "bla"
w ((x:xs):xxs) | eqBool False (head (map (eqChar '(') (x:xs))) = w xs
             | True = concat ((x:xs):xxs)

-- xs should be [xs]
-- 13,68-69
