module Practicum1 where

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
writeTable t = concat(printlijst ( (map (\x -> combi2(vulAanLijst(aantalSpatiesLijst t x) (schrijfRijNr x t))) [1..n]) ++ [sluitTabel (length(t)) t]))
  where n = length (transpose t)

project :: [String] -> Table -> Int
project (x:xs) ((y:ys):yss) | (x eqString y) = 2
                            | otherwise      = 3




lengtelijst :: [[a]] -> [Int]
lengtelijst [] = []
lengtelijst ([]:y) = lengtelijst y
lengtelijst ((x:xs):xss) = length(x:xs):lengtelijst xss

lengteElement :: Table -> [Int]
lengteElement x = map maximum (map lengtelijst (transpose x))








lengteWoord :: String -> Int
lengteWoord []     = 0
lengteWoord (_:xs) = 1 + lengteWoord xs

sluitTabel :: Int -> Table -> [String]


sluitTabel n t = (["+\n"] ++ (schrijfSluitstuk t (n+1)) ++ ["+\n"])

schrijfSluitstuk :: Table -> Int -> [String]
schrijfSluitstuk _ 0 = []
schrijfSluitstuk t n | (length(t)==n) = "+\n" : (schrijfSluitstuk t (n-1))
                     | otherwise      = "|\n" : (schrijfSluitstuk t (n-1))

schrijfRijNr :: Int -> Table -> [String]
schrijfRijNr _ ([] : _)     = []
schrijfRijNr _ []           = []
schrijfRijNr n ((a:as):ass) = ((a:as) !! (n-1)):(schrijfRijNr n ass)

vulAan :: Int -> String -> String
vulAan 0 y = y
vulAan x y = (y)++schrijfSpaties x

schrijfSpaties :: Int -> String
schrijfSpaties 0 = []
schrijfSpaties n = " " ++ schrijfSpaties (n-1)

aantalSpatiesLijst :: Table -> Int -> [Int]
aantalSpatiesLijst t n = map (((lengteElement t) !! (n-1)) -) (map lengteWoord (schrijfRijNr n t))



vulAanLijst :: [Int] -> [String] -> [String]
vulAanLijst (_ : _) []    = []
vulAanLijst [] _          = []
vulAanLijst (n:ns) (l:ls) = (["|" ++ vulAan n l])++(vulAanLijst ns ls)

combi2 :: [String] -> [String]
combi2 []     = []
combi2 (n:ns) = (([schrijfLijn (lengteWoord n)]) ++ [n] ++ ([schrijfLijn (lengteWoord n)]) ++ ns ++ ([schrijfLijn (lengteWoord n)]))




printlijst :: [[String]] -> [String]
printlijst xs = map concat (transpose xs)





















schrijfLijn :: Int -> String
schrijfLijn x = "+" ++ (schrijfMinnen (x-1))

schrijfMinnen :: Int -> String
schrijfMinnen 0 = []
schrijfMinnen n = ("-")++schrijfMinnen(n-1)

schrijfStreep :: String -> String
schrijfStreep a = "|" ++ a

-- should be: eqString x y
-- 50,32-32   50,32-43
