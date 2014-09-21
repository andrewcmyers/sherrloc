module Linda where
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



getMaxWidths :: Table -> [Int]
getMaxWidths table = maxWidth (transpose table)

maxWidth :: Table -> [Int]
maxWidth [] = [0]
maxWidth (x:xs) = (maximum (map length x):  maxWidth xs)



writeTable :: Table -> IO ()
writeTable x = putStr (writeLineTable x)







writeLineTable :: Table -> [String]
writeLineTable x = concatMap (`replicate1` '-') columns
   where columns = getMaxWidths x





replicate1 :: Int -> Char -> [Char]
replicate1 n x = ['+'] ++ take (n) (repeat x)

-- signature should be: Table -> [Char]
-- 50,19-35   50,28-35   50,1-54    50,29-34
