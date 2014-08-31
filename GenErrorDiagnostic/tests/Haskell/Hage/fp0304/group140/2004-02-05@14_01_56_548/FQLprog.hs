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




writeTable :: Table -> String
writeTable tabel = (unlines (voegInHorLijn (breedtes) (map plaatsStreepje (vulAan1 tabel (breedtes)))))
           where breedtes = maxKolomBreedte tabel


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
voegInHorLijn _ [] = []
voegInHorLijn lijst (rij:rijen) = streep : rij : streep : (rijen ++ [streep])
   where streep :: String
         streep = tekenHorLijn lijst






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







select :: String -> (String -> Bool) -> Table -> Table
select _ _ []   = []
select veld conditie (xs:xss) = xs : [ rij | rij<- xss , indexInt>=0, conditie (rij!!indexInt)]
        where indexMaybe = findIndex (eqString veld) xs
              indexInt   = maybeToInt indexMaybe


maybeToInt :: Maybe Int -> Int
maybeToInt Nothing  = (-1)
maybeToInt (Just i) = i









join :: Table -> Table -> Table
join t1 t2 = [x++(verwijderElem y index2) | x <- t1, y <- t2, predicaat x y]
                where gemveld = gemVeld t1 t2
                      predicaat s1 s2 = eqString (zoekOp s1 index1) (zoekOp s2 index2)
                      index1 = maybeToInt (findIndex (eqString gemveld)(head t1))
                      index2 = maybeToInt (findIndex (eqString gemveld)(head t2))
                      zoekOp [] _ = ""
                      zoekOp (x:_) 0 = x
                      zoekOp (_:xs) t = zoekOp xs (t-1)
                      verwijderElem (_:xs) 0 = xs
                      verwijderElem (x:xs) t = x:(verwijderElem xs (t-1))
                      verwijderElem [] _ = error


gemVeld :: Table -> Table -> String
gemVeld [] [] = ""
gemVeld [] _  = ""
gemVeld _ []  = ""
gemVeld t1 t2 | (not.null)(filter (eqString x) (head t2) ) = concat (filter (eqString x) (head t2))
              | otherwise                                  = concat (filter (eqString (head xs)) (head t2))
                where (x:xs) = (head t1)


query :: Table
query =
    project ["Compiler"]
      (select "Soort" (eqString "Universiteit")
        (join compilers (join locaties soort)))

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

-- error should be: error ""
-- 135,44-48
