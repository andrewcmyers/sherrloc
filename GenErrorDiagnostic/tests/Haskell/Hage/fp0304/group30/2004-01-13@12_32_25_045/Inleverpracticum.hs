module Inleverpracticum where
import Data.List
type Table = [[String]]

compilers :: Table
compilers =
 [
 ["Compiler", "Universiteit/bedrijf"]
 ,["Helium", "Universiteit van Utrecht"]
 ,["NHC", "University of York"]
 ,["GHC", "Microsoft Research"]
 ,["Hugs", "Galois Connections"]
 ,["Hugs.NET", "Galois Connections"]
 ,["O'Haskell", "Oregon Graduate Institute"]
 ,["O'Haskell", "Chalmers University of Technology"]
 ,["HBC", "Chalmers University of Technology"]
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

col table= head (transpose table)

lengthTable :: [a] -> [Int]
lengthTable [] =  [0]
lengthTable (x:xs) = [length x] ++ [length xs]

-- signature should be [[a]] -> [Int]
-- 32,16-27   32,1-12   32,16-18    32,17-17
