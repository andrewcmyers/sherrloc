module Opdracht1(module List, module Opdracht1) where

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




breedteKolom :: Table -> [Int]
breedteKolom (tabel) = lengthString (transpose (tabel))




lengthString :: Table -> [Int]
lengthString ([]) = []
lengthString (lijstEen : lijstRest) =  map length (head(lijstEen : lijstRest)) ++ lengthString (lijstRest)




vulAan :: Int -> String -> String
vulAan aantal tekst = take aantal (tekst ++ repeat ' ')




maakRegel :: [String] -> [Int] -> String
maakRegel [] []       ='|'
maakRegel (tekst:restTekst) (aantal:restAantal)  = '|' ++
                                                   vulAan aantal (head (tekst:restTekst)) ++
                                                   maakRegel restTekst restAantal

-- both '|' should be "|"
-- 54,24-26   55,52-54
