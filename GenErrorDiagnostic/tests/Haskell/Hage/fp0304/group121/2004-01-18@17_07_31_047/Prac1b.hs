module Prac1b where

import Data.List

type Table = [[String]]

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


writeTable :: Table -> String
writeTable [] = []
writeTable l@(x:xs) = streep l ++ slaPlat (spatieLijst (langsten l) x) ++ streep l ++ concatMap slaPlat (spatieTable xs) ++ streep l




slaPlat :: [String] -> String
slaPlat rij = slaPlat2 rij  ++ "|\n"
        where slaPlat2 :: [String] -> String
              slaPlat2 [] = []
              slaPlat2 (x:xs) = "|" ++ x ++ slaPlat2 xs



streep :: Table -> String
streep tabel = streep2 (langsten tabel) ++ "+\n"
       where streep2 :: [Int] -> String
             streep2 [] = []
             streep2 (x:xs) = "+" ++  (concat $ replicate (x ) "-" ) ++ streep2 xs



spatieTable :: Table -> Table
spatieTable tabel = map (spatieLijst (langsten tabel)) tabel

voegIn :: Int -> String ->String
voegIn n woord= woord ++  (concat $ replicate (n - (length woord)) " " )

spatieLijst :: [Int] -> [String] -> [String]
spatieLijst [] [] = []
spatieLijst (_ : _) [] = []
spatieLijst [] (_ : _) = []
spatieLijst (n:ns) (y:ys) = voegIn n y : spatieLijst ns ys


grootsteLengte :: [String] -> Int
grootsteLengte lijst =  maximum $ map length lijst

langsten :: Table -> [Int]
langsten tabel = map grootsteLengte $ transpose tabel







project :: [String] -> Table -> Table
project [] _ = []
project keuze tabel = transpose (selector keuze tabel)

selector :: [String] -> Table -> Table
selector [] _ = []
selector (x:xs) tab = selectCol x tab :  selector xs tab

eqFirstString :: String -> [String] -> Bool
eqFirstString _ [] = False
eqFirstString y (z:_) | eqString y z = True
                      | otherwise = False

selectCol :: String -> Table -> [String]
selectCol _ [] = []
selectCol woord tabel = concat (filter(eqFirstString woord) (transpose tabel))




select :: String -> (String -> Bool) -> Table -> Table
select woord p tabel = lijstFilter ( welkeRijen woord p tabel) tabel

welkeRijen :: String -> (String -> Bool) -> Table -> [Bool]
welkeRijen woord p tab = True : map p (tail (selectCol woord tab))

lijstFilter :: [Bool] -> Table -> Table
lijstFilter [] _ = []
lijstFilter _ [] = []
lijstFilter (x:xs) (y:ys) | x = y: lijstFilter ys
                          | otherwise = lijstFilter ys

-- missing paramter "xs" for both lijstFilter 
-- 112,36-46   112,36-49   113,41-51   113,41-54
