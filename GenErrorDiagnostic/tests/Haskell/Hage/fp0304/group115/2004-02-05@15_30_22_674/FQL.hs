module FQL where

import Data.List

type Table = [[String]]

eqChar      :: Char -> Char -> Bool
eqChar = undefined

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




writeTable    :: Table -> String

voegIn        :: Table -> Table

dwarsbalk     :: [String] -> [String]

kitkat        :: [String] -> String

voegstreeptoe :: [String] -> [String]

equalize      :: Table -> Table

ultraErect    :: [String] -> [String]

erect         :: [String] -> [String]

fillUp        :: String -> Int -> String





writeTable tabel     = foldr (++) "" (map  kitkat(voegIn (map voegstreeptoe (equalize tabel))))

voegIn (x:xs)        = dwarsbalk x : (x:(dwarsbalk x : (sluitxsaf xs)))

sluitxsaf staart     = reverse (dwarsbalk (head staart) : reverse staart)

dwarsbalk []         = []
dwarsbalk (x:xs)     = ("+" ++ (replicate (length x) '-' )) ++ dwarsbalk xs

kitkat lijst         | (eqChar '|' (head(head lijst))) = foldr (++) "|\n" lijst
                     | (eqChar '+' (head(head lijst))) = foldr (++) "+\n" lijst

voegstreeptoe []     = []
voegstreeptoe (x:xs) = ("|" ++ x) : voegstreeptoe xs

equalize tabel       =  (transpose (map ultraErect (transpose tabel)))

ultraErect lijst     = reverse (erect (reverse (erect lijst)))


erect (x1: [])       = (x1: [])
erect (x1: (x2: xs)) |  length x1>length x2 = (x1: erect ((fillUp x2 (length x1-length x2)): xs))
                     |  length x1<length x2 = ((fillUp x1 (length x2-length x1)): erect (x2 : xs))
                     |  otherwise           = (x1: erect(x2: xs) )


fillUp woord 0 = woord
fillUp woord verschil = fillUp (woord++" ") (verschil-1)

-- ++ should be :
-- 66,61-62
