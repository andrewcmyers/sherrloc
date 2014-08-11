module Fql(module List, module Fql) where
import Data.List

type Table = [[String]]
eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined
index :: Int -> [a] -> a
index = undefined

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



select :: Int -> [[a]] -> [a]
select kolom tabel = map (index kolom) tabel

breedte :: Int -> [[[a]]] -> Int
breedte kolom tabel = maximum (map (length) (select kolom tabel))

eerstekol :: [[a]] -> [a]
eerstekol tabel = map (index 1) tabel


mapp :: (a->b) -> [[a]] -> [[b]]
mapp f = map (map f)


breedtes :: [[[a]]] -> [Int]
breedtes tabel = map maximum (mapp length (transpose tabel))


verschil :: [Int] -> [Int]
verschil rij = map (maximum rij -) rij

verschilPerKol :: [[[a]]] -> [[Int]]
verschilPerKol tabel = transpose (kolomVerschil tabel)


kolomVerschil :: [[[a]]] -> [[Int]]
kolomVerschil tabel = map verschil (mapp length (transpose tabel))


aantalKolommen :: [[a]] -> Int
aantalKolommen tabel = length (transpose tabel)

aantalRijen :: [[a]] -> Int
aantalRijen tabel = length (tabel)


veld :: Int -> Int -> [[[Char]]] -> [Char]
veld rij kolom tabel = concatMap (++ ( (replicate (verschilPerVak rij kolom tabel) ' ') ++ "|")) (lines(vak rij kolom tabel))

maxKolBreedte :: Int -> [[[a]]] -> Int
maxKolBreedte kol tabel = maximum (map length (index kol (transpose tabel)))



verschilPerVak :: Int -> Int -> [[[a]]] -> Int
verschilPerVak r k tabel = (maxKolBreedte k tabel) - (length (index k (index r tabel)))

vak :: Int -> Int -> [[a]] -> a
vak rij kolom tabel = index kolom (index rij tabel)

hoofding :: [[[Char]]] -> [Char]
hoofding tabel = ' ': rijLayout 0 tabel

matrix :: [[[Char]]] -> [Char]
matrix tabel = ' ' : unwords[rijLayout rij tabel| rij <- [1..((aantalRijen tabel)-1 )]]

rijLayout :: Int -> [[[Char]]] -> [Char]
rijLayout rij tabel = "|" ++ (concatMap (++ "") (rijVelden rij tabel)) ++ "\n"

rijVelden :: Int -> [[[Char]]] -> [[Char]]
rijVelden rij tabel = [(veld rij y tabel)|y <- [0..((aantalKolommen tabel)- 1)]]



lijnHor :: [[[a]]] -> [Char]
lijnHor tabel = ' ' : "+" ++ (concatMap (++ "+") [(replicate x '-')|x <- (breedtes tabel)]) ++ "\n"

writeTable tabel = lijnHor tabel ++ hoofding tabel ++ lijnHor tabel ++  matrix tabel ++ lijnHor tabel
writeTable :: [[[Char]]] -> [Char]

tabelletje :: [[[Char]]] -> IO ()
tabelletje tabel = putStr (writeTable tabel)

lijst :: [a] -> [a]
lijst tabel = map ((!!)  tabel) [1..((length tabel) - 1 )]





indexen inputString  tabel = findIndices (eqBool True) (map (eqString inputString) (titels tabel))

titels tabel = [(vak 0 y tabel)|y <- [0..((aantalKolommen tabel)- 1)]]

test inputString = findIndices (eqBool True) [(map (eqString inputString) (titels locaties))]

-- missing [] around (map (eqString inputString) (titels tabel))
-- 118,56-98
