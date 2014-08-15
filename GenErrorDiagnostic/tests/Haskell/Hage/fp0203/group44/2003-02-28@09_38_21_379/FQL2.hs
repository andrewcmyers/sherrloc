module FQL2(module List, module FQL2) where

import Data.List
eqString      :: String -> String -> Bool 
eqString = undefined
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

soort :: Table
soort =
  [ ["Universiteit/bedrijf", "Soort"]
  , ["Universiteit van Utrecht", "Universiteit"]
  , ["University of York", "Universiteit"]
  , ["Microsoft Research", "Bedrijf"]
  , ["Galois Connections", "Bedrijf"]
  , ["Oregon Graduate Institute", "Universiteit"]
  , ["Chalmers University of Technology", "Universiteit"]
  ]











lengteTabelElementen :: Table -> [[Int]]
lengteTabelElementen tabel = map (map length ) tabel






zoekKolomLangsten :: Table -> [Int]
zoekKolomLangsten tabel = map maximum (transpose (lengteTabelElementen tabel))







vulOp :: String -> Int -> String
vulOp s i = take i (s++repeat ' ')






streep :: [Int] -> String
streep [] = "+\n"
streep (x:xs) = "+" ++ take x (repeat '-') ++ streep xs







schrijfRegel :: [Int] -> [String] -> String
schrijfRegel [] [] = "|\n"
schrijfRegel [_] [] = []
schrijfRegel [] [_] = []
schrijfRegel (x:xs) (y:ys) = "|" ++ (vulOp y x) ++ (schrijfRegel xs ys)




writeTable :: Table -> String
writeTable []     = "lege tabel"
writeTable lijst =     streep (zoekKolomLangsten lijst)
                    ++ schrijfRegel (zoekKolomLangsten lijst) (head lijst)
                    ++ streep (zoekKolomLangsten lijst)
                    ++ concat (map (schrijfRegel (zoekKolomLangsten lijst)) (tail lijst))
                    ++ streep (zoekKolomLangsten lijst)













geefKolomIndexen :: Table -> [String] -> [Int]
geefKolomIndexen  _ [] = []
geefKolomIndexen  tabel (y:ys) |or identiek   = length (takeWhile not identiek):(geefKolomIndexen tabel ys)
                               |otherwise     = geefKolomIndexen tabel ys
                               where identiek = map (eqString y) (head tabel)





geefKolom :: Table -> Int  -> [String]
geefKolom tabel x = map (!!x) tabel






project :: [String] -> Table -> Table
project lijst tabel = transpose( map (geefKolom tabel) (geefKolomIndexen tabel lijst))












veldIndex :: Table -> String -> Int
veldIndex tabel veldnaam = head (geefKolomIndexen tabel [veldnaam])






vergelijkIndex :: Int -> (String -> Bool) -> [String]  -> Bool
vergelijkIndex volgNr f lijst  = f (lijst!!volgNr)







select :: String -> (String -> Bool) -> Table -> Table
select veldnaam f tabel = head tabel : (filter (vergelijkIndex (veldIndex tabel veldnaam) f) tabel)







gemeenschappelijk :: Table -> Table -> [Int]
gemeenschappelijk tabel1 tabel2 = geefKolomIndexen tabel1 (head tabel2) ++ (geefKolomIndexen tabel2 (head tabel1))

cartes :: Table -> Table -> Table
cartes (x:xs) tabel2 = map ((++)x) tabel2 :(cartes xs tabel2)






leeg :: Table
leeg = []

test1 :: IO()
test1 = putStr (writeTable compilers)

test2 :: IO()
test2 = putStr (writeTable (project ["Land","Universiteit/bedrijf","onzin_f3y4","Land","Land",""] locaties))

test3 :: IO()
test3 = putStr (writeTable (select "Compiler" ((==3) . length) compilers))

-- : should be: ++
-- 178,43-43
