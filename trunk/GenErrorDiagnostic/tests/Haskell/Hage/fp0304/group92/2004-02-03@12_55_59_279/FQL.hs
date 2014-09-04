module FQL where

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

lengthsLine::[String] -> [Int]
lengthsTable::Table -> [[Int]]
maxLengthsTable::Table -> [Int]
writeBorder::[Int] -> String

writeBorderPart :: Int -> String

writeCell::String->Int->String






lengthsLine l = map length l
lengthsTable t = map lengthsLine t
maxLengthsTable t = foldr1 (zipWith max) (lengthsTable t)
writeBorder t = concat ["+",concat (map writeBorderPart t)]
writeBorderPart x = concat [(replicate x '-'),"+"]
writeCell cell lengthz = concat [cell,replicate (lengthz-length cell) ' ',"|"]
writeTable t = concat [writeBorder m,y,writeBorder m]
        where   m = maxLengthsTable t
                y = concat (writeLine t)
                writeLine line = concat ["|",concat (zipWith writeCell line m)]

-- should be: (map writeLine t)
-- 51,29-37   51,29-39
