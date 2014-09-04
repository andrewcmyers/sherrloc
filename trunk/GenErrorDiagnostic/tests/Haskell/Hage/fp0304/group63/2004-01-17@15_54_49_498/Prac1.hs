module Prac1 where

import Data.List

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


writeTable :: Table -> String
writeTable (t:ts) = maaks (breedteKolommen (t:ts)) ++ "+ \n" ++
                    write t (breedteKolommen (t:ts)) ++
                    maaks (breedteKolommen (t:ts)) ++ "+ \n" ++
                    write ts(breedteKolommen (t:ts)) ++
                    maaks (breedteKolommen (t:ts)) ++ "+ \n"

write :: Table -> [Int] -> String
write [] (_:_) = []
write (_:_) [] = []
write [] [] = []
write (t:ts) breedte = schrijfLijn t breedte ++"| \n" ++ write ts breedte


lengte :: Table -> [[Int]]
lengte [] =[]
lengte (x:xs)= map length x : lengte xs


breedteKolommen :: Table -> [Int]
breedteKolommen y = map maximum (transpose (lengte y))







maaks :: [Int] -> String
maaks []=[]
maaks (y:ys) = "+" ++ (replicate y '-') ++ (maaks ys)


schrijfLijn :: [String] -> [Int] -> String
schrijfLijn(y:ys) (b:bs) = "|" ++ y ++ vulAan b y  ++ schrijfLijn ys bs
schrijfLijn [] (_:_) = []
schrijfLijn (_:_) [] = []
schrijfLijn [] [] = []


vulAan :: Int -> String -> String
vulAan b y = replicate n ' '
           where n = b - length y

-- t should be: [t]
-- 34,27-27
