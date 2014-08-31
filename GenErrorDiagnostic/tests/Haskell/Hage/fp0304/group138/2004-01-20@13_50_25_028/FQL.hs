module FQL where

import Data.List

type Table = [[String]]

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





















eerste_rij::[[String]] -> [String]
eerste_rij [] = []
eerste_rij x = head x

aantal_kolommen :: [[String]] -> Int
aantal_kolommen [] = 0
aantal_kolommen tabel = length (eerste_rij (tabel))


n_de_kolom :: [[String]]-> Int -> [String]
n_de_kolom []     _ = []
n_de_kolom (x:xs) n = [(!!) x n] ++ n_de_kolom xs n






lengte_strings:: [[String]]-> [[Int]]
lengte_strings [] = [[0]]
lengte_strings (x: xs) =  (map length x) : lengte_strings xs



max_breedte::[[Int]] -> [Int]
max_breedte lijst = map maximum lijst





combineer::[[String]] -> [Int]
combineer table =  max_breedte(transpose(lengte_strings table))


vul_aan::[String] -> [Int] -> [String]
vul_aan [] [] = []
vul_aan [] _ = [[]]
vul_aan xs [] = xs
vul_aan (x:xs) (y:ys)= ( "|" ++ x ++ (take (y-(length x)) (repeat ' '))): vul_aan xs ys



tabel_vul_aan::[[String]]-> [Int]-> [[String]]
tabel_vul_aan [] breedtes = []
tabel_vul_aan (x:xs) breedtes = vul_aan (x:xs) breedtes : tabel_vul_aan xs breedtes


unlines1:: [[String]] -> [String]
unlines1 [] = []
unlines1 (l:ls) = l ++ "" : unlines1 ls






concat_rij:: String -> String
concat_rij rij =  rij ++ "|"


rij_af :: [String] -> String
rij_af rij = concat_rij (concat rij)

tabel_af:: [[String]] -> [String]
tabel_af [] = []
tabel_af (x:xs) = rij_af x : tabel_af xs

writeTable:: [[String]] -> String
writeTable table = unlines(tabel_af (tabel_vul_aan table (combineer table)))

-- (x:xs) should be x
-- 96,42-45
