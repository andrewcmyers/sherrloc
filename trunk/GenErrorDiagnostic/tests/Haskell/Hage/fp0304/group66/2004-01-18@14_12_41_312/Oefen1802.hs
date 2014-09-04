module Oefen1802 where

sp :: [[a]] -> [a]
sp [] = []
sp (k:s) =  k ++ sp s

keerom :: [a]->[a]
keerom [] = []
keerom (kop:staart) = keerom staart ++ [kop]

vrec :: (a->a->Bool)->[a] -> a -> [a]
vrec _ [] _ = []
vrec vgl (x:xs) a |x vgl a = vrec vgl xs a
                  |otherwise = x :(vrec vgl xs a)

-- x vgl a should be: vgl x a
-- 13,20-26   13,20-20
