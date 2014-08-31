module FQLDael where

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
writeTable tab = schrijfBegrenzing br  ++
                 schrijfLijn br str    ++
                 schrijfBegrenzing br  ++
                 recSchrijfLijn br tabje ++
                 schrijfBegrenzing br
                             where br = breedteKolom tab
                                   str = head tab
                                   tabje = tail tab



recSchrijfLijn :: [Int] -> Table -> String
recSchrijfLijn  [] []     = []
recSchrijfLijn (_ : _) [] = []
recSchrijfLijn [] (_ : _) = []
recSchrijfLijn (y:ys) (x:xs) = schrijfLijn (y:ys) x ++ recSchrijfLijn (y:ys) xs




schrijfLijn :: [Int] -> [String] -> String
schrijfLijn [] _ = "|\n"
schrijfLijn _ [] = []
schrijfLijn (y:ys) (x:xs) = '|' : (vulAan x y) ++ schrijfLijn ys xs



breedteKolom :: Table -> [Int]
breedteKolom = (map (langsteWoorden.(map length))).transpose



langsteWoorden :: [Int] -> Int
langsteWoorden [] = 0
langsteWoorden [x] = x
langsteWoorden (x:(y:xs))
                | (x >= y)   = langsteWoorden (x:xs)
                | otherwise  = langsteWoorden (y:xs)



vulAan :: String -> Int-> String
vulAan kortWoord wensLengte = kortWoord ++ (replicate n ' ')
           where n = wensLengte - length kortWoord




schrijfBegrenzing :: [Int] -> String
schrijfBegrenzing [] =  "+\n"
schrijfBegrenzing (x:xs) = '+' : (replicate x '-') ++ schrijfBegrenzing xs





project :: [String] -> Table -> Table
project [] _ = []
project (x:xs) y = zoekTabelstuk (zoekPlaatsen (x:xs) (head y)) y



zoekPlaats :: String -> [String] -> Int
zoekPlaats _ [] = 0
zoekPlaats y (x:xs)
                | eqString y x = 1
                | otherwise = 1 + zoekPlaats y xs



zoekPlaatsen :: [String] -> [String] -> [Int]
zoekPlaatsen [] _ = []
zoekPlaatsen (y:ys) x = zoekPlaats y x : zoekPlaatsen ys x



zoekString :: [Int] -> [String] -> [String]
zoekString [] _ = []
zoekString (x:xs) y = (y !! (x-1)) : zoekString xs y



zoekTabelstuk :: [Int] -> Table -> Table
zoekTabelstuk _ [] = []
zoekTabelstuk x (y:ys) = zoekString x y : zoekTabelstuk x ys




select :: String -> (String -> Bool) -> Table -> Table
select _ _ [] = []
select kolomnaam conditie (x:xs)  |conditie (pakJuiste (x) (hoeVeelste kolomnaam (x))) = x: select kolomnaam conditie xs
                                  |otherwise =  select kolomnaam conditie (x)



hoeVeelste :: String -> [String] -> Int
hoeVeelste _ [] = 0
hoeVeelste x (y:ys)   | eqString x y = 0
                      | otherwise = 1+ hoeVeelste x ys


pakJuiste :: [String]  -> Int -> String
pakJuiste [] _ = []
pakJuiste (x:xs) y    | y == 0 = x
                      | otherwise = pakJuiste xs (y-1)

-- x should be: xs
-- 127,76-76
