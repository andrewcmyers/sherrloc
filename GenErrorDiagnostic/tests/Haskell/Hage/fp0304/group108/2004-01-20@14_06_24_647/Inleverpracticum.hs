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


lengthCell :: [String] -> Int
lengthCell strings = maximum (map length strings)

maxLengthCell:: Table -> [Int]
maxLengthCell table= map lengthCell (transpose table)


makeLines :: [Int] -> String
makeLines [] = "+\n"
makeLines (x:xs) = "+"
                   ++ replicate x '-'
                   ++ makeLines xs

makeRows :: String -> String
makeRows [] = "|\n"
makeRows (x:xs) = "|"
                   ++ x
                   ++ "|"
                   : xs
                   ++ "|"

-- ++ at line 48 should be :    : at line 49 should be ++
-- 48,20-21   49,20-20
