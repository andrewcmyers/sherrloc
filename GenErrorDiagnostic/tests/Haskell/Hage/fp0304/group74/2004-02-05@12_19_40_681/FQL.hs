module FQL where

import Data.List

eqString      :: String -> String -> Bool 
eqString = undefined

type Table  = [[String]]




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







lengteString2 :: Table -> [[Int]]
lengteString2 [] = []
lengteString2 (x:xs) = [map length x] ++ lengteString2 xs

horzvert :: Table -> [[Int]]
horzvert x = transpose(lengteString2 x)



maxString :: Table ->[Int]
maxString [] = [0]
maxString x = map maximum(horzvert x)




lengteString :: [String] -> [Int]
lengteString [] = []
lengteString (x:xs) = length x : lengteString xs

langsteString :: [String] -> Int
langsteString [] = 0
langsteString (x) = maximum(lengteString x)








aanvullen :: Table -> Table
aanvullen (x) = transpose (map aanvullen3 (transpose x))

aanvullen2 :: Table -> Table
aanvullen2 [] = []
aanvullen2 (x:xs) = aanvullen3 y x : aanvullen2 xs
                    where y = x

aanvullen3 :: [String] -> [String] -> [String]
aanvullen3 _ [] = []
aanvullen3 y (x:xs) = take (langsteString y) (x ++ repeat ' ') : aanvullen3 y xs




verticaleStrepen :: [String] -> [Char]
verticaleStrepen [] = "|"
verticaleStrepen (x:xs) = "|" ++ x ++ verticaleStrepen xs

verticaleStrepen2 :: Table -> [String]
verticaleStrepen2 x = map verticaleStrepen (aanvullen x)





aantalStreepjes :: Int -> String
aantalStreepjes x = "+" ++ replicate x '-'

lijnTrekken :: Table -> String
lijnTrekken x = concat( map aantalStreepjes ( maxString x) ++ ["+"])

horizontaleStreep :: Table -> [String] -> [String]
horizontaleStreep y (x:xs) = (lijnTrekken y) : x : (lijnTrekken y) : horizontaleStreep2 y xs

horizontaleStreep2 :: Table -> [String] -> [String]
horizontaleStreep2 y [] = [lijnTrekken y]
horizontaleStreep2 y (x:xs) =  x : horizontaleStreep2 y xs



writeTable :: Table -> IO ()
writeTable x = putStr (unlines(horizontaleStreep x (verticaleStrepen2 x)))





project :: [String] -> Table -> Table
project x y = transpose (project2 x (transpose y))

project2 :: [String] -> Table -> Table
project2 [] _ = []
project2 (x:xs) y = vergelijk x y : project2 xs y


vergelijk :: String -> Table -> [String]
vergelijk _ [] = []
vergelijk x (y:ys) | eqString x (head y) = y
                   | otherwise = vergelijk x ys

plakLijst :: Table -> String -> [String]
plakLijst x y = tail (concat(project [y] x))

pasToe :: String -> (String -> Bool) -> Table -> [String]
pasToe x y z = (filter y (plakLijst z x))


nrElement :: String -> [String] -> Int
nrElement _ [] = 0
nrElement x (y:ys) | eqString x y = 0
                   | otherwise = 1 + nrElement x ys

zetInputOm :: Table -> [String] -> [Int]
zetInputOm _ [] = []
zetInputOm (x:xs) (y:ys) = (nrElement y x) : zetInputOm (x:xs) ys

rijNummer :: String -> Table -> [String] -> [Int]
rijNummer x y z = 0: (zetInputOm [(concat(project [x] y))] z)

goedPrinten :: Table -> [Int] -> Table
goedPrinten _ [] = []
goedPrinten x (y:ys) = x !! y : goedPrinten x ys

select :: String -> (String -> Bool) -> Table -> Table
select x y z = goedPrinten z (rijNummer x z (pasToe x y z))

-- should delete 'map'
-- 75,28-55   75,28-30   75,32-41
