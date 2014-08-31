module Database where
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






maxWidth :: Table -> [Int]
maxWidth tabel = map f t
               where t = transpose tabel
                     f kolom = maximum (map length kolom)

writeLines :: [Int] -> String
writeLines []     = "+\n"
writeLines (x:xs) = "+" ++ replicate x '-'
                        ++ writeLines xs

writeWords :: [Int] -> [String] -> String
writeWords [_:_] []         = "|\n"
writeWords (x:xs) (y:ys) = "|" ++
                           y ++
                           replicate (x - (length y)) ' '  ++
                           writeWords xs ys

writeTable :: Table -> String
writeTable table = (writeLines (maxWidth table))++
                   (writeWords (maxWidth table) (table !! 0)) ++
                   (writeLines (maxWidth table))++
                   (concatMap (writeWords (maxWidth table)) (tail table)) ++
                   (writeLines (maxWidth table))

-- [_:_] should be: [_]
-- 47,12-16   47,13-15
