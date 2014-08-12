module Fql (module List, module Fql) where
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
writeTable tabel = drop 1 (concat(voegNewLineIn (voegLijnToe (transpose ( maakBeginLijn (vulAllesAan (transpose tabel ) ) ) ) )))


voegNewLineIn :: Table -> [String]
voegNewLineIn [] = []
voegNewLineIn (x:xs) | null (xs) = "\n":x
                     | otherwise = ("\n":x) ++ (voegNewLineIn xs)


kolomBreedtes :: [String] -> [Int]
kolomBreedtes [] = []
kolomBreedtes (x:xs) = (length x):(kolomBreedtes xs)


vulAan :: Int -> String -> String
vulAan lengte string  | length string == lengte      = string++"|"
                      | otherwise          = vulAan lengte (string++" ")


vulAllesAan :: Table -> Table
vulAllesAan [] = []
vulAllesAan (xs:xss)  | null xs    = []
                      | otherwise = (vulAanLijst (maximum(kolomBreedtes xs)) xs):(vulAllesAan xss)
                        where vulAanLijst lengte string = map (vulAan lengte) string


maakBeginLijn :: Table -> Table
maakBeginLijn [] = []
maakBeginLijn (xs:xss) = (map ('|' :) xs):xss


maakLijn :: Table -> String
maakLijn [] = "+"
maakLijn (xs:xss) = "+" ++ (streep(maximum(kolomBreedtes xs))) ++ (maakLijn xss)
                      where streep lengte = concat(replicate (lengte-1) "-")


voegLijnToe :: Table -> Table
voegLijnToe [] = []
voegLijnToe (xs:xss) = [lijn]:xs:[lijn]:(xss++[[lijn]])
                     where lijn = gooiTeVeelWeg ( maakLijn(transpose (xs:xss) ) )
                           gooiTeVeelWeg string = "+"++(drop 2 string)





project :: [String] -> Table -> Table
project string table = transpose ((selecteerKolom string (transpose table)))


pakGelijke :: String -> Table -> Table
pakGelijke _ [] = []
pakGelijke string    (x:xs) | null x                       = []
                            | eqString string (head x)     = x:(pakGelijke string xs)
                            | otherwise                    = pakGelijke string xs


selecteerKolom :: [String] -> Table -> Table
selecteerKolom [] _ = []
selecteerKolom _ [] = []
selecteerKolom (x:xs) table | null x    = []
                            | otherwise = (pakGelijke x table)++(selecteerKolom xs table)







select :: String -> (String -> Bool) -> Table -> Table
select string conditie table = ( (head table):(conditieChecker (kolomZoeken string (transpose table)) conditie table ) )


kolomZoeken :: String -> Table -> Int
kolomZoeken _ [] = 5
kolomZoeken string (x:xs) | null x              = 5
                          | eqString string (head x) = 1
                          | otherwise           = 1 + kolomZoeken string xs


conditieChecker :: Int -> (String -> Bool) -> Table -> Table
conditieChecker _ _ [] = []
conditieChecker int conditie (x:xs) | null x         = []
                           | int > (length x) = []
                           | conditie ( x!!(int-1) ) = x:(conditieChecker int conditie xs)
                           | otherwise      = conditieChecker int conditie xs







plakTabellen :: Table -> Table -> Table
plakTabellen table1 [] = table1
plakTabellen [] table2 = table2
plakTabellen table1 table2 = (transpose table1)++(transpose table2)


gelijkeKolom :: Table -> Table -> String
gelijkeKolom table1 table2 = concat [ x | x<-(head table1), y<-(head table2), eqString x y ]


verwijderKolom :: Table -> String -> Int -> Table
verwijderKolom [] _ _ = []
verwijderKolom (x:xs) string int | null x = []
                                 | (eqString string (head x) ) && (int == 0)= x:(verwijderKolom xs string (int+1))
                                 | (eqString string (head x) ) && (int /= 0)= (verwijderKolom xs string int)
                                 | otherwise                = x:(verwijderKolom xs string int)





kruisProdukt :: Table -> Table -> Table
kruisProdukt [] table2 = table2
kruisProdukt table1 [] = table1
kruisProdukt table1 table2 = [ x++y | x <- table1, y <- table2]

kolom :: Table -> Table -> Table -> Table
kolom _ _ [] = []
kolom table1 table2 (x:xs) | null x = []
                           | eqString (gelijkeKolom table1 table2) (head x) = x: kolom table1 table2 xs
                           | otherwise = kolom table1 table2 xs

kolomNaarEen :: Table -> [String]
kolomNaarEen [] = []
kolomNaarEen (x:xs) | null x = []
                    | eqString (head x) (last x) = x:kolomNaarEen xs
                    | otherwise = kolomNaarEen xs

-- should be: ++
-- 171,53-53
