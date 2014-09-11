module AantalOpl where
aantalOpl :: Float -> Float -> Float -> Float -> Float
aantalOpl a b c = ((- b + sqrt (b * b - 4.0 * a * c)) / ( 2.0 * a ))
                  ((- b - sqrt (b * b - 4.0 * a * c)) / ( 2.0 * a ))

-- sig should be Float -> Float -> Float -> [Float]; the expression should be [..., ...]
-- 2,1-9   2,14-54   2,41-54   3,19-4,68
