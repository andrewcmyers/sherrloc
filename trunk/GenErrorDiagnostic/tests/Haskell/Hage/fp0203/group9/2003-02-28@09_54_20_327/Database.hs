module Database(module List, module Database) where

import Data.List

type Table = [[String]]

eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqChar      :: Char -> Char -> Bool
eqChar = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined


compilers :: Table
compilers = [ ["Compiler", "Universiteit/bedrijf"]
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
locaties = [ ["Universiteit/bedrijf", "Land", "Stad"]
  , ["Universiteit van Utrecht", "Nederland", "Utrecht"]
  , ["University of York", "Engeland", "York"]
  , ["Microsoft Research", "Engeland", "Cambridge"]
  , ["Galois Connections", "Verenigde Staten", "Beaverton"]
  , ["Oregon Graduate Institute", "Verenigde Staten", "Beaverton"]
  , ["Chalmers University of Technology", "Zweden", "Goteborg"]
  ]






grootste :: [Int] -> Int
grootste [] = 0
grootste (x : xs) | x <= grootste xs = grootste xs
                  | x > grootste xs = x


lengte :: [String] -> [Int]
lengte x = map length x



maxBreedteKolom :: [String] -> Int
maxBreedteKolom kolom = grootste (lengte kolom)



maxBreedte :: Table -> [Int]
maxBreedte tabel = map maxBreedteKolom (transpose tabel)








streep1 :: Int -> String
streep1 x = replicate x '-'



alleStreep :: Table -> String
alleStreep tabel = plus (map streep1 (maxBreedte tabel))


plus :: [[Char]] -> [Char]
plus [] = ""
plus [woord] = woord ++ "+\n"
plus (woord:ws) = woord ++ '+' : plus ws



kop :: Table -> [Char]
kop tabel = "+" ++ alleStreep tabel ++ tabelLijnen tabel ++ "+" ++ alleStreep tabel



tabelLijnen :: Table -> [Char]
tabelLijnen (x:_) = opmaak x



opmaak :: [String] -> String
opmaak [] = ""
opmaak [woord] = "|" ++ woord ++ "|\n"
opmaak (woord:ws) = "|" ++ woord ++ opmaak ws



layout0 :: Table -> Table
layout0 tabel = transpose (layout0_1 (transpose tabel))


layout0_1 :: Table -> Table
layout0_1 [] = []
layout0_1 ((_ : rest) : resten) = rest : layout0_1 resten


layout1 :: [String] -> [Char]
layout1 [] = "|"
layout1 (woord : overig) = "|" ++ woord ++ layout1 overig


layout2 :: Table -> [[Char]]
layout2 tabel = map layout1 (layout0 tabel)


layout3 :: [[Char]] -> [Char]
layout3 [] = ""
layout3 (rij : restrijen) = rij ++ "\n" ++ layout3 restrijen



alleLayout :: Table -> [Char]
alleLayout tabel = layout3 (layout2 tabel)


vulAan2 :: String -> String
vulAan2 woord = woord ++ repeat ' '



vulAan4 :: [String] -> [String] -> [String]
vulAan4 _ [] = []
vulAan4 kolom (woord : rest) = take (maxBreedteKolom kolom) (vulAan2 woord) : vulAan4 kolom rest


vulAan5 :: Table -> Table
vulAan5 [] = []
vulAan5 (kolom : overig) = (vulAan4 kolom kolom) : vulAan5 overig



vulAan6 :: Table -> Table
vulAan6 tabel = vulAan5 (transpose tabel)




vulAllesAan :: Table -> Table
vulAllesAan tabel = transpose (vulAan6 tabel)



writeTable :: Table -> [Char]
writeTable tabel = kop (vulAllesAan tabel) ++ alleLayout (vulAllesAan tabel) ++ "+" ++ alleStreep tabel








visKolom :: String -> Table -> [String]
visKolom _ [] = []
visKolom kolomnaam tabel | eqString kolomnaam (unwords(take 1 (head tabel))) = (head tabel)
                         | otherwise = visKolom kolomnaam (tail tabel)



visAlleKolom :: [String] -> Table -> Table
visAlleKolom [] _ = []
visAlleKolom (kolomnaam : rest) tabel = (visKolom kolomnaam (transpose tabel)) : (visAlleKolom rest tabel)



project :: [String] -> Table -> Table
project kolomnamen tabel = transpose (visAlleKolom kolomnamen tabel)








select1 :: String -> (String -> Bool) -> Table -> [String]
select1 kolomnaam booleanFunctie tabel = filter booleanFunctie (visKolom kolomnaam (transpose tabel))


select1_5 :: String -> [String] -> [Bool]
select1_5 _ [] = []
select1_5 testwoord (woord : woorden) | eqString testwoord woord = True : (select1_5 testwoord woorden)
                                      | otherwise = False : (select1_5 testwoord woorden)


select1_75 :: [Bool] -> Bool
select1_75 [] = False
select1_75 (boolean : rest) | eqBool boolean True = True
                            | eqBool boolean False = select1_75 rest


select1_875 :: String -> [String] -> Bool
select1_875 testwoord rij = select1_75 (select1_5 testwoord rij)



select2 :: [String] -> Table -> Table
select2 [] _ = []
select2 _ [] = []
select2 (woord : woorden) (rij : rijen) | eqBool (select1_875 woord  rij) True = rij : select2 woorden rijen
                                        | otherwise = select2 (woord : woorden) rijen


select3 :: String -> (String -> Bool) -> Table -> Table
select3 kolomnaam booleanFunctie tabel = select2 (select1 kolomnaam booleanFunctie tabel) tabel


select4 :: [String] -> [String]
select4 kolom = take 1 kolom


select5 :: Table -> [String]
select5 tabel = concatMap select4 (transpose tabel)


select :: String -> (String -> Bool) -> Table -> Table
select kolomnaam booleanFunctie tabel = (select5 tabel) : (select3 kolomnaam booleanFunctie tabel)




































join1 :: [String] -> Table -> Int
join1 lijst (kolom: kolommen) | eqList eqChar lijst kolom = 1
                              | otherwise = 1 + (join1 lijst kolommen)













soort :: Table
soort = [ ["Universiteit/bedrijf", "Soort"]
  , ["Universiteit van Utrecht", "Universiteit"]
  , ["University of York", "Universiteit"]
  , ["Microsoft Research", "Bedrijf"]
  , ["Galois Connections", "Bedrijf"]
  , ["Oregon Graduate Institute", "Universiteit"]
  , ["Chalmers University of Technology", "Universiteit"]
  ]

-- eqChar should be eqString
-- 271,40-45
