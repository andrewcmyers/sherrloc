module FQLprog where

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



maxKolomBreedte :: Table -> [Int]
maxKolomBreedte tabel= map maximum (map (map length) (transpose tabel))



tekenHorLijn :: [Int] -> String
tekenHorLijn [] = "+"
tekenHorLijn (x:xs) = "+" ++ replicate x '-' ++ tekenHorLijn xs


plaatsStreepje :: [String] -> String
plaatsStreepje [] = "|"
plaatsStreepje (x:xs) = "|" ++ x ++ plaatsStreepje xs


vulAan1 ::  Table -> [Int] -> Table
vulAan1  tabel lijst = map (vulAan2 lijst) tabel


vulAan2 :: [Int] -> [String] -> [String]
vulAan2 lijst rij  =  zipWith f lijst rij
        where f :: Int -> String -> String
              f getal woord = woord ++ replicate (getal- length woord)  ' '


voegInHorLijn :: [Int] -> [String] -> [String]
voegInHorLijn lijst (rij:rijen) = streep : rij : streep : (rijen ++ [streep])
   where streep :: String
         streep = tekenHorLijn lijst



writeTable :: Table -> String
writeTable tabel = (unlines (voegInHorLijn (breedtes) (map plaatsStreepje (vulAan1 tabel (breedtes)))))
           where breedtes = maxKolomBreedte tabel






project :: [String] -> Table -> Table
project kolomnamen tabel = transpose (projectAnders kolomnamen tabel)




pakGoedeKolom :: String -> [String] -> [String]
pakGoedeKolom kolomnaam kolom | eqString (head kolom) kolomnaam  = kolom
                              | True  = []


pakDezeKolom :: String -> Table -> [String]
pakDezeKolom kolomnaam tabel = concatMap (pakGoedeKolom kolomnaam) (transpose tabel)



projectAnders :: [String] -> Table -> Table
projectAnders [] _       = []
projectAnders (x:xs) tabel   = pakDezeKolom x tabel : projectAnders xs tabel







pakIndex :: Maybe Int -> Int
pakIndex Nothing = -1
pakIndex (Just i)= i

select :: String -> (String -> Bool) -> [[String]] -> [[String]]
select _ _ []   = []
select veld conditie (xs:xss) = xs : [ ys | ys<- xss , m>=0, conditie (ys!!m)]
        where n = findIndex (eqString veld) xs
              m = pakIndex n


gemeenschappelijk ::  [[String]] -> [[String]] -> String
gemeenschappelijk tabel1 tabel2 | eqString (head(head tabel1))(head(head tabel2)) = head (head tabel1)
                                | otherwise                                       = gemeenschappelijk (head tabel1) (tail (head tabel2))

-- missing []
-- 116,118-135
