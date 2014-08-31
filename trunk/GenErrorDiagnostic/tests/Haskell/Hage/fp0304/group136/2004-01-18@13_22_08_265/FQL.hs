module FQL where

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
writeTable x =  (hLine a) ++ (writeTable2 x a True) ++ (hLine a)
  where a = maxTable x

writeTable2 :: Table -> [Int] -> Bool -> String
writeTable2  [] _ _= ""
writeTable2 (x:xs) y z  | z = "|" ++ (writeH x y) ++  "\n"++ (hLine y )++ (writeTable2 xs y False)
                        | otherwise  = "|" ++ (writeH x y) ++  "\n"++ (writeTable2 xs y False)

writeH :: [String] -> [Int] ->   String
writeH [] [] = ""
writeH (_ : _) [] =  ""
writeH [] (_ : _) =  ""
writeH  (x:xs) (y:ys) = (putSpaces x y) ++ ( writeH xs ys)

putSpaces :: String -> Int ->   String
putSpaces x y = x ++ take (y-length x) (repeat ' ') ++ "|"

hLine :: [Int] -> String
hLine [] = "+\n"
hLine (x:xs) = "+" ++ (hoLine x)++(hLine xs)

hoLine :: Int -> String
hoLine x | x==0 = ""
         | otherwise = "-" ++ hoLine(x-1)

turnTable :: Table ->  Table
turnTable x | length (head x) == 1 = [map head x]
            | otherwise = map head x:(turnTable (map tail x))

maxLength :: [String] -> Int
maxLength x = maximum (map length x )

maxTable :: Table -> [Int]
maxTable x = map maxLength (turnTable x)

project :: [String] -> Table -> Table
project [] [_] = []
project [] (_ : (_ : _)) = []
project [] [] = []
project (x:xs) a = (projKol x b ) ++( project xs  b)
        where b = turnTable a

projKol :: String -> Table -> [String]
projKol _ [] = []
projKol a (x:xs) | eqString a  (head x) = x
                 | otherwise = projKol a xs

-- ++ should be: ':'
-- 77,35-36
