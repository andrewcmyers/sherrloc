module Aantalopl where
kwadraat :: Int -> Int
kwadraat x = x*x

aantalOpl :: Float -> Float -> Float -> Int
aantalOpl a b c | (kwadraat b - 4*a*c)<0 = 0
                | (kwadraat b - 4*a*c)==0 = 1
                | (kwadraat b - 4*a*c)>0 = 2

-- signature should be: Int -> Int -> Int -> Int
-- 5,14-43
