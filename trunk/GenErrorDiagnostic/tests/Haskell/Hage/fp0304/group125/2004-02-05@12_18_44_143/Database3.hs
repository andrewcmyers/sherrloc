module Database3 where

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
writeTable table = unlines $ map lijntjesvert $ transpose $ map kolommen $ transpose table

kolommen :: [String] -> [String]
kolommen [] =[]
kolommen kolom = lijntjes (breedte kolom) (uitlijnen (breedte kolom) kolom)
                  where breedte = maximum . map length


uitlijnen :: Int -> [String] -> [String]
uitlijnen _ [] = []
uitlijnen breedte (x:xs) = (x ++ replicate (breedte  - length x) ' ') : uitlijnen breedte xs


lijntjes :: Int -> [String] -> [String]
lijntjes _ [] = []
lijntjes breedte (x:xs) = lijn : x : lijn : xs ++ lijn : []
                  where lijn = replicate (breedte) '-'

lijntjesvert :: [String] -> String
lijntjesvert [] = " "
lijntjesvert [w] | eqString "--" (take 2 w) = '+' : w ++ "+"
                 | otherwise = '|' : w ++ "|"
lijntjesvert (w:ws) | eqString "--" (take 2 w) = '+' : w ++ lijntjesvert ws
                    | otherwise = '|' : w ++ lijntjesvert ws



project :: [String] -> Table -> Table
project [] [] = []
project s t = transpose(vergelijk s(transpose t)(transpose t))

vergelijk :: [String] -> Table -> Table -> Table
vergelijk (_ : _) ([] : _) _ = []
vergelijk (_ : _) [] _ = []
vergelijk [] (_ : _) _ = []
vergelijk [] [] _ = []
vergelijk (x:xs) ((y:ys):yss) z | eqString x y = (y:ys):vergelijk xs z z
                                | True         = vergelijk (x:xs) yss z















zoekVeldnaam :: String -> Table -> Int

zoekVeldnaam _ ([] : _) = 0
zoekVeldnaam _ [] = 0
zoekVeldnaam s ((x:xs):xss) | eqString s x = 1
                            | otherwise = zoekVeldnaam s xs

-- xs should be: xss
-- 95,58-59
