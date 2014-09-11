module Prac1 (module List, module Prac1) where

import Data.List
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
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

grootsteInt :: [Int] -> Int
grootsteInt [] = 0
grootsteInt [x] = x
grootsteInt (x:(y:xs)) | (x >= y) = grootsteInt (x:xs)
                       | otherwise = grootsteInt (y:xs)

kolomBreedtes :: Table -> [Int]
kolomBreedtes = (map (grootsteInt.(map length))).transpose

aanVul :: Int -> String -> String
aanVul n tekst = take n (tekst ++ repeat ' ')

tabelRegel :: [Int] -> [String] -> String
tabelRegel [] [] = "|\n"
tabelRegel (y:ys) (x:xs) = "|" ++ aanVul y x ++ tabelRegel ys xs

tabelVeld :: Table -> Table -> String
tabelVeld _ [] = []
tabelVeld y (x:xs) = tabelRegel (kolomBreedtes y) x ++ tabelVeld y xs

rij :: Int -> String
rij 0 = "+"
rij x = "-" ++ (rij (x-1))

tabelScheiding :: [Int] -> String
tabelScheiding [] = "+\n"
tabelScheiding (x:xs) = reverse(rij x) ++ tabelScheiding xs

veldNaam :: Table -> [String]
veldNaam x = head x

veldRest :: Table -> Table
veldRest x = tail x

writeTable :: Table -> String
writeTable x = tabelScheiding (kolomBreedtes x) ++
               tabelVeld x [veldNaam x] ++
               tabelScheiding (kolomBreedtes x) ++
               tabelVeld x (veldRest x) ++
               tabelScheiding (kolomBreedtes x)

ndeString :: String -> [String] -> Int
ndeString _ [] = 0
ndeString y (x:xs) | eqString y x = 1
                    | otherwise = 1 + ndeString y xs

lijstNdeString :: [String] -> [String] -> [Int]
lijstNdeString [] _ = []
lijstNdeString (y:ys) (x:xs) = ndeString y (x:xs) : lijstNdeString ys (x:xs)

zoveelsteElement :: Int -> [String] -> String
zoveelsteElement x (y:ys) | x==1 = y
                          | x/=1 = zoveelsteElement (x-1) ys

gewensteStrings :: [Int] -> [String] -> [String]
gewensteStrings [] _ = []
gewensteStrings (x:xs) y = (zoveelsteElement x y) : gewensteStrings xs y

gewensteTabel :: [Int] -> Table -> Table
gewensteTabel _ [] = []
gewensteTabel x (y:ys) = gewensteStrings x y : gewensteTabel x ys

project :: [String] -> Table -> Table
project [] _ = []
project (x:xs) y = gewensteTabel (lijstNdeString (x:xs) (veldNaam y)) y


gewensteStringsUitKolom :: String -> (String -> Bool) -> Table -> [String]
gewensteStringsUitKolom x f tabel = filter f (concat(project [x] tabel))

zoekString :: [String] -> [String] -> [String]
zoekString [] _  = []
zoekString (x:xs) (y:ys) | eqString x y = (y:ys)
                         | otherwise = zoekString xs (y:ys)

dropLeeg :: Table -> Table
dropLeeg [] = []
dropLeeg (x:xs) | eqList eqString x [] = dropLeeg xs
                | otherwise = x : dropLeeg xs

voegToe :: [String] -> Table -> Table
voegToe _ [] = []
voegToe (x:xs) (y:ys) = dropLeeg((zoekString (x:xs) y) : voegToe (x:xs) ys)

select :: String -> (String -> Bool) -> Table -> Table
select x f tabel =  (voegToe [x] tabel) ++ voegToe (gewensteStringsUitKolom x f tabel) tabel

gelijk1 :: String -> [String] -> String
gelijk1 _ [] = []
gelijk1 x (y:ys) | (eqString x y) = x
                 | otherwise = gelijk1 x ys

gelijk2 :: [String] -> [String] -> String
gelijk2 [] _ = []
gelijk2 (x:xs) y = ((gelijk1 x y)++(gelijk2 xs y))


zelfdeKolomNaam :: Table -> Table -> Int
zelfdeKolomNaam x y = gelijk2 (head x) (head y)


vergelijk :: String -> [String] -> [String]
vergelijk [] _ = []
vergelijk _ [] = []
vergelijk x (y:ys) | eqString x y = (y:ys)
                   | otherwise = vergelijk x ys

vergelijk2 :: String -> Table -> [String]
vergelijk2 _ [] = []
vergelijk2 x (y:ys) = vergelijk x y ++ vergelijk2 x ys

vind :: String -> [String] -> [String]
vind x [] = []
vind x (y:ys) | eqString x y = (y:ys)
              | otherwise = y : (vind x ys)

vinddeel :: String -> Table -> Table
vinddeel _ [] = []
vinddeel [] _ = []
vinddeel x (y:ys) = vind x y : (vinddeel x ys)

vindgeheel :: [String] -> Table -> Table
vindgeheel [] _ = []
vindgeheel _ [] = []
vindgeheel (x:xs) y = vinddeel x y ++ (vindgeheel xs y)

sorteer :: [String] -> Table -> Table
sorteer [] _ = []
sorteer (x:xs) tabel2 = ((vergelijk2 x tabel2) : (sorteer xs tabel2))

-- Table -> Table -> Int should be: Table -> Table -> String
-- 131,1-15   131,20-40   131,38-40
