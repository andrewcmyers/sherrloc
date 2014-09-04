module Databeestje where

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



writeHeader :: Table -> String
writeHeader list  = neemIntList(maxLengte2 list)

neemIntList :: [Int] -> String
neemIntList [] = "+" ++ "\n"
neemIntList (h:tl) = "+" ++ (concat(replicate h "-")) ++ neemIntList tl

mL :: [String] -> [Int]
mL [] = []
mL (h:tl) = length h : mL tl

listmL :: Table -> [[Int]]
listmL [] = []
listmL list = mL (head  list) : listmL (tail list)

listmL1 :: Table -> [[Int]]
listmL1 list = transpose (listmL list)

maxLengte2 :: Table -> [Int]
maxLengte2 list = map maximum (listmL1 list)

writeTableName :: Table -> String
writeTableName [] = error "De tabel moet een naam hebben"
writeTableName list = writeTuple (head list) (maxLengte2 list)

writeTuple :: [String] -> [Int] -> String
writeTuple (_:_) [] = error "Er is geen lengte bekend om te uilijning te bepalen"
writeTuple [] (_:_) = error "Er zijn geen woorden af te beelden"
writeTuple [] [] = "|" ++ "\n"
writeTuple (h:tl) (f:fl) = "|" ++ h ++ concat(replicate (f - (length h)) " ") ++ writeTuple tl fl

writeData :: Table -> String
writeData [] = ""
writeData (_:[]) = ""
writeData list = (writeTuple  (head (lijstOnthoofd list)) (maxLengte2 list)) ++ writeData (tail list)

lijstOnthoofd :: Table -> Table
lijstOnthoofd [] = []
lijstOnthoofd (_:fl) = fl

writeTable :: Table -> String
writeTable [] = error "Er zijn gegevens af te beelden"
writeTable list = writeHeader list ++ writeTableName list ++ writeHeader list ++ writeData list ++ writeHeader list










geefTable :: String -> Table -> Table
geefTable _ [] = []
geefTable woord table | eqString woord (head(head(transpose table))) = head(transpose table) : []
                      | not(eqString woord (head(head(transpose table)))) = geefTable woord  (tail(transpose table))


geefTableWoordenLijst :: [String] -> Table -> Table
geefTableWoordenLijst [] _ = []
geefTableWoordenLijst _ [] = []
geefTableWoordenLijst woorden table = geefTable (head woorden) table ++ geefTableWoordenLijst (tail woorden) table

geefTransposedTable :: [String] -> Table -> Table
geefTransposedTable _ [] = []
geefTransposedTable woorden table = transpose (geefTableWoordenLijst woorden table)

geefTable1 :: [String] -> Table -> Table
geefTable1 _ [] = []
geefTable1 [] _ = []
geefTable1 woord table | eqString (head woord) (head(head(transpose table))) = head(transpose table) : geefTable1 (tail woord) (tail table)
                       | otherwise = geefTable1 (tail woord) (tail table)























geefTerug :: String -> Table -> [String]
geefTerug woord table | eqString woord (head(head(transpose table))) = head (transpose table)

geefTerug1 :: String -> Table -> Table
geefTerug1 woord table = geefTerug woord table : []

geefTerug2 :: String -> Table -> Table
geefTerug2 woord table = transpose (geefTerug1 woord table)

geefTerug3 :: [String] -> Table -> Table
geefTerug3 _ [] = []
geefTerug3 [] _ = []
geefTerug3 (x:xl) table = geefTerug2 x table : geefTerug3 xl table

-- : should be ++
-- 144,46-46
