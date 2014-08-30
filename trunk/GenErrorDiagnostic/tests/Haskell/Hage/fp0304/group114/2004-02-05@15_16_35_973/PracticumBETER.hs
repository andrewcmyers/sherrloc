module PracticumBETER where

import Data.List

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined

type Table = [[String]]
tabelLengte        :: Table -> [Int]
writeTable         :: Table -> String
maakEenStringVan   :: String -> Char-> Char-> Int -> String
maakEenTabel       :: Table -> [Int] -> Table
project            :: [String] -> Table -> Table
nummerKolom        :: String -> Table -> Int





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



tabelLengte g = map kolomLengte (transpose g)
     where kolomLengte h = maximum(map length h)

maakEenStringVan string char nogeenchar int = char:string++(replicate (int-length string) nogeenchar)

maakEenTabel [] [] = [[]]
maakEenTabel (a:resta) (b:restb) = [(maakKolom a b)]++(maakEenTabel resta restb)
     where maakKolom (c:restc) d = [(maakEenStringVan c '|' ' ' d)]++(maakKolom restc d)
           maakKolom [] _ = []
maakEenTabel [] (_:_) = [[]]
maakEenTabel (_:_) [] = [[]]


writeTable [] = "Welke tabel?"
writeTable (a:resta) = concat(addRegel++(schrijf [a] (a:resta))++addRegel++(schrijf resta (a:resta))++addRegel)
           where schrijf x y = concat(map(++["| \n"]) (transpose(maakEenTabel (transpose x) (tabelLengte y))))
                 addRegel = (map(maakEenStringVan "-"'+''-') (tabelLengte (a:resta)))++["+ \n"]



nummerKolom _ []   = 1
nummerKolom string (x:rest)   | (eqString string (head x))=0
                              | otherwise=1+nummerKolom string rest



project veld tabel = transpose (map(getrTabel !!)(map(`nummerKolom` getrTabel) veld))
        where getrTabel    = transpose(tabel)




select :: String -> (String -> Bool) -> Table -> Table

select _ _ [] = []
select kolom f (x:xs) = x: filter p xs
             where p list = f (list !! (nummerKolom kolom (x:xs)))






overEenKomst :: [String] -> [String] -> Bool


overEenKomst (_:_) [] = []
overEenKomst [] _ = []
overEenKomst (x:xs) (y:ys) | eqString x y = True
                           | otherwise = overEenKomst xs ys

-- both [] should be False
-- 95,25-26   96,21-22
