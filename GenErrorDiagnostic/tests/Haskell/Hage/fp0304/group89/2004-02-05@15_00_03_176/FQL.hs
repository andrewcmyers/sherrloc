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


maxWidth :: Table -> [Int]
maxWidth tabel = map f t
              where t = transpose tabel
                    f kolom = maximum ( map length kolom )

breedteKolom :: Table -> Int -> Int
breedteKolom tabel kolom = maxWidth tabel !! (kolom-1)

writeHead :: Table -> String
writeHead tabel = "+" ++ concatMap writeMinnetjes breedte
             where breedte = maxWidth tabel

writeMinnetjes :: Int -> String
writeMinnetjes 0 = "+"
writeMinnetjes x = "-" ++ writeMinnetjes (x-1)

aantalRijen :: Table -> Int
aantalRijen tabel = length ( head ( transpose tabel ) )

addSpace :: String -> Int -> String
addSpace string n
              |  length string /= n = ( addSpace string (n-1) ) ++ " "
              |  otherwise = "|" ++ string

stringSpaceHead :: Table -> String
stringSpaceHead tabel = concat ( zipWith addSpace string breedtes ) ++ "|"
                             where string   = head tabel
                                   breedtes = maxWidth tabel

stringSpaceTail :: Table -> [Int] -> String
stringSpaceTail [] _ = []
stringSpaceTail (x:xs) breedtes =  f ++ "\n" ++ g
                       where f = concat ( zipWith addSpace x breedtes ) ++ "|"
                             g = stringSpaceTail xs breedtes

writeRestTabel :: Table -> String
writeRestTabel tabel = stringSpaceTail ( tail tabel ) ( maxWidth tabel )

writeHeaders :: Table -> String
writeHeaders tabel = concat [ a++"\n", b++"\n", a++"\n" ]
                             where a = writeHead tabel
                                   b = stringSpaceHead tabel

writeTable :: Table -> String
writeTable tabel = a ++ b ++ c
                   where a = writeHeaders tabel
                         b = writeRestTabel tabel
                         c = writeHead tabel

selectKolommen :: [String] -> [[String]] -> [[String]]
selectKolommen [] _ = []
selectKolommen (x:xs) tabel = a:b
                          where a = selectKolom x tabel
                                b = selectKolommen xs tabel

selectKolom :: String -> [[String]] -> [String]
selectKolom string tabel = head ( filter conditie ( transpose tabel ) )
                                         where conditie :: [String] -> Bool
                                               conditie kolom = eqString ( head kolom ) string

kolomNummer :: String -> [String] -> Int
kolomNummer kolomnaam namen
                           | eqString kolomnaam ( head namen ) = 0
                           | otherwise = 1 + kolomNummer kolomnaam ( tail namen )

project :: [String] -> Table -> Table
project string tabel = transpose ( selectKolommen string tabel )

select :: String -> (String -> Bool) -> Table -> Table
select kolom conditie tabel = head tabel : ( filter conditie2 (tail tabel ) )
                                        where conditie2 rij = conditie ( rij !! ( kolomNummer kolom ( head tabel ) ) )

bepaalDubbel :: [[String]] -> [[String]] -> [String]
bepaalDubbel tabel1 tabel2 = [ x | x<-( head tabel1 ), y<- ( head tabel2 ), eqString x y ]

join :: Table -> Table -> Table
join tabel1 tabel2 = [ x++y | x<-compilers, y<-locaties, eqString (x!!kn1) (y!!kn2) ]
                          where kn1 = kolomNummer dub ( head tabel1 )
                                kn2 = kolomNummer dub ( head tabel2 )
                                dub = bepaalDubbel tabel1 tabel2

-- bepaalDubbel should return String; line 108 should be: head [...]
-- 107,17-52   108,30-90
