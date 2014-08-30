module Test where
data Expr
     = Con Int
     | Expr :+ Expr
     | Var String



data Boom2 a = Blad2 a
             | Tak (Boom2 a) (Boom2 a)

diepte :: Boom2 a -> Int
diepte (Blad2 a) = 1
diepte (Tak li re) = max (1 + diepte li) (1 + diepte re)

length3 :: [a] -> Int
length3 lijst = foldr (+1) 0 lijst

-- (+1) should be: \x y . y + 1
-- 17,23-26   17,24-25 
