module Database where
import Data.List

type Table = [[String]]

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined

compilers :: Table
compilers =
  [ ["Compiler", "Universiteit/bedrijf"]
  , ["Helium", "Universiteit van Utrecht"]
  , ["NHC", "University of York"]
  , ["GHC", "Microsoft Research"]
  , ["Hugs", "Galois Connections"]
  , ["Hugs.NET", "Galois Connections"]
  , ["O'Haskell", "Oregon Graduate Institute"]
  , ["O'Haskell", "Chalmers University of Technology"]
  , ["HBC", "Chalmers University of Technology"]
  ]

locaties :: Table
locaties =
  [ ["Universiteit/bedrijf", "Land", "Stad"]
  , ["Universiteit van Utrecht", "Nederland", "Utrecht"]
  , ["University of York", "Engeland", "York"]
  , ["Microsoft Research", "Engeland", "Cambridge"]
  , ["Galois Connections", "Verenigde Staten", "Beaverton"]
  , ["Oregon Graduate Institute", "Verenigde Staten", "Beaverton"]
  , ["Chalmers University of Technology", "Zweden", "Goteborg"]
  ]






maxWidth :: Table -> [Int]
maxWidth tabel = map f t
               where t = transpose tabel
                     f kolom = maximum (map length kolom)








writeLines :: [Int] -> String
writeLines [] = "+\n"
writeLines (x:xs) = "+"
                    ++ replicate x '-'
                    ++ writeLines xs










writeWoorden :: [Int] -> [String] -> String
writeWoorden [] [] = "|\n"
writeWoorden (x:xs) (y:ys) = "|"
                             ++ y
                             ++ replicate (x-(length y))' '
                             ++ writeWoorden xs ys








writeTable :: Table -> String
writeTable tabel  =  writeLines x
                    ++ writeWoorden x(head tabel)
                    ++ writeLines x
                    ++ concat(map (writeWoorden x) (tail tabel))
                    ++ writeLines x
                   where x = maxWidth tabel










project :: [String] -> Table -> Table
project namen tabel = transpose (map f namen)
                   where f x = head(filter p (transpose tabel))
                             where p y = eqString (head y) x











veldNummer :: [String] -> String -> Int
veldNummer (x:xs) veld | eqString x veld = 0
                       | otherwise = 1 + veldNummer xs veld






select :: String -> (String -> Bool) -> Table -> Table
select veld conditie (x:xs) = x : (filter p xs)
                            where p y = conditie (y !! (veldNummer x veld))







gemVeldnaam t1 t2 = head [x | x <- head t1,elemBy eqString x (head t2)]

koppelRijen t1 t2 = [x ++ y|x <- t1, y <- t2, eqString (x !! n) (y !! m)]
                  where n = veldNummer t1 (gemVeldnaam t1 t2)
                        m = veldNummer t2 (gemVeldnaam t1 t2)

-- t1 should be (head t1); similar for t2
-- 135,40-41   136,40-41
