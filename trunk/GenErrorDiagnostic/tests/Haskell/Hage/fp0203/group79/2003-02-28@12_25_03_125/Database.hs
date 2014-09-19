module Database(module List, module Database) where

import Data.List

type Table = [[String]]

eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

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

soort :: Table
soort =
  [ ["Universiteit/bedrijf", "Soort"]
  , ["Universiteit van Utrecht", "Universiteit"]
  , ["University of York", "Universiteit"]
  , ["Microsoft Research", "Bedrijf"]
  , ["Galois Connections", "Bedrijf"]
  , ["Oregon Graduate Institute", "Universiteit"]
  , ["Chalmers University of Technology", "Universiteit"]
  ]



writeTable :: Table -> String
writeTable [[]] = []
writeTable tabel = plusjes (langste(transpose tabel)) ++ "\n" ++
                   rijen (transpose tabel)(langste(transpose tabel)) ++ "\n" ++
                   plusjes (langste(transpose tabel)) ++ "\n" ++
                   bodyTable2 tabel (langste(transpose tabel))++
                   plusjes (langste(transpose tabel))

project :: [String] -> Table -> Table
project [] _ = [[]]
project velden tabel =  transpose (sorteren tabel velden)

select :: String -> (String -> Bool) -> Table -> Table
select veld conditie tabel = (head tabel) : (init (temp3 veld conditie tabel))






langste :: Table -> [Int]
langste [] = []
langste (x:xs) = maximum (map length x):(langste xs)

plusjes :: [Int] -> String
plusjes [] = "+"
plusjes (x:xs) = '+' : (take x(repeat '-')  ) ++ plusjes xs

vulAan :: Int -> String -> String
vulAan n tekst = take n (tekst ++ repeat ' ')

rijen :: Table -> [Int] -> String
rijen [] _ = "|"
rijen (x:xs) (y:ys) = "|" ++ vulAan y (head x) ++ (rijen xs ys)

bodyTable :: Table -> [Int] -> [String]
bodyTable [] _ = []
bodyTable (_:xs) lang = rijen (transpose xs)(lang): bodyTable (xs)(lang)

bodyTable2 :: [[String]] -> [Int]-> String
bodyTable2 tabel lang= unlines(init(bodyTable tabel lang))



sortKolom :: [String] -> String -> Int
sortKolom [] _ = 1
sortKolom stringa stringb | eqString (head stringa) stringb = 1
                          | otherwise = 1 + sortKolom  (tail stringa) stringb

selecteren :: Table -> [String] -> [String]
selecteren tabel veld | (sortKolom (head tabel)(head veld)) > length (head tabel) = []
                      | otherwise = last (take (sortKolom (head tabel)
                                    (head veld)) (transpose tabel))

sorteren :: Table -> [String] -> Table
sorteren _ [] = []
sorteren tabel veld = selecteren tabel veld : sorteren tabel (tail veld)



sortRij :: String -> (String -> Bool) -> Table -> [String]
sortRij veld conditie tabel = filter conditie (tail (selecteren  tabel [veld]))

rijAanvul :: String -> String -> Table -> Int
rijAanvul veld naam tabel = sortKolom (concat (sorteren tabel [veld])) naam

temp2 :: Table -> String -> [String]
temp2 [] _ = []
temp2 (x:xs) naam | null (filter (eqBool True) (map (eqString naam) x))= temp2 xs naam
                  | head (filter (eqBool True) (map (eqString naam) x))= x

temp4 :: String -> (String -> Bool) -> Table -> [[String]]
temp4 veld conditie tabel = (head tabel):(drop (rijAanvul veld (head
                            (sortRij veld conditie tabel)) tabel) tabel)

temp3 :: String -> (String -> Bool) -> Table -> [[String]]
temp3 _ _ [[]] = [[]]
temp3 veld conditie tabel | null (sortRij veld conditie tabel) = [[]]
                          | True = temp2 tabel (head (sortRij veld conditie tabel)):
                                   temp3 veld conditie (temp4 veld conditie tabel)



gemeen :: [String] -> [String] -> String
gemeen [] _ = []
gemeen tabel1 tabel2 | null (temp2 [tabel2] (head tabel1)) = gemeen (tail tabel1) tabel2
                     | otherwise = (head tabel1)

temp5 :: Table -> Table -> Table
temp5 tabel1 tabel2 = project [gemeen (head tabel1) (head tabel2)] tabel1

temp6 :: Table -> Table -> Table -> Table
temp6 tabel1 tabel2 = (select (gemeen (head tabel1) (head tabel2)) (eqString (head(head(temp5 tabel1 tabel2)))) tabel2)






















hio :: [[String]]
hio = [["hoi","hoi2"],["","hallo"],["hoi",""]]

hio2 :: [[String]]
hio2 = [["hoi"]]

-- signature should be Table -> Table -> Table
-- 140,1-5    140,10-41   140,28-41    140,37-41
