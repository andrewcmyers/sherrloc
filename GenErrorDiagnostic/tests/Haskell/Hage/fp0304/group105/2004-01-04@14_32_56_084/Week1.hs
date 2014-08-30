module Week1 where

aantalOpl::Float -> Float -> Float -> Int

aantalOpl a b c       | d > 0.0 = 2
                      | d < 0.0 = 0
                      | d == 0.0 = 1
                      where d = b * b - 4.0 * a * c

abcFormule:: Float -> Float -> Float -> [Float]
abcFormule a b c = [ (- b + d)/n
                   , (-b - d)/n
                   ]
                   where d = sqrt(b * b - 4.0 * a * c)
                         n = 2.0*a

aantalOpl2::Float -> Float -> Float -> Int
aantalOpl2 a b c = length  abcFormule a b c


-- missing () around abcFormule a b c
-- 18,20-43   18,20-25
