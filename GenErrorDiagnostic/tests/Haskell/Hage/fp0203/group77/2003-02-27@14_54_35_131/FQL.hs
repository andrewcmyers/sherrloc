module FQL(module List, module FQL) where

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

writeTable :: Table -> String
writeTable x = unlines (map concat (y))
           where y =  unikol x











uniform  ::  [String] -> [String]
uniform  xs  =  evenlang  n  xs
  where  n = maximum (map length xs)

evenlang  ::  Int -> [String] -> [String]
evenlang  n  xs  = (("+" ++ replicate n '-') : map  (precies n)  xs) ++ ("+" : replicate n '-')

precies  ::  Int -> String -> String
precies  n  s
    | n < len = "|" ++ take  n  s
    | n == len = "|" ++ s
    | n > len = "|" ++ s ++ replicate (n-len) ' '
    where len = length s

unikol :: [[String]] -> [[String]]
unikol = transpose.map uniform.transpose

-- '-' should be "-"
-- 50,92-94
