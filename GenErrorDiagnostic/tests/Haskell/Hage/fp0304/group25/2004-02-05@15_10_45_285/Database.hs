module Database where

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


locaties2 :: Table
locaties2 =
 [ ["Zweden"]
  ]


flat :: [[a]] -> [a]
flat [] = []
flat (x:xs) = head (x:xs)


count :: [Int] -> [Int]
count [] = [0]
count (x:[]) = [x]
count (x:xs) = count ((filter (>x)(filter (>=x)(xs))) ++ [x])

kolom :: [[a]] -> [Int]
kolom [] = [0]
kolom (x:[]) = [length x]
kolom (x:xs) = count (map length ( (x:xs)))


writeTable :: Table -> String
writeTable [[]] = []
writeTable [] = " "


writeTable (x:xs) = (borders (x:xs) ++ "+" ++ "\n" ++ body kol (x:[]) ++ borders (x:xs) ++ "+" ++ "\n" ++ body kol (xs) ++ borders (x:xs) ++ "+" ++ "\n")


                  where kol = (x:xs)


borders :: [[String]] -> String
borders [[]] = ""
borders [] = ""
borders (x:xs) = ('+' : (replicate (head (kolom (head (transpose (x:xs))))) '-')) ++ (borders (transpose (tail (transpose (x:xs)))))


body :: [[String]] -> [[String]] -> [Char]

body k [] = ""

body k (x:xs) = ("|" ++ line k (x:xs) ++ "\n") ++ body k xs

line :: [[String]] -> [[String]] -> String
line k [[]] = ""
line k [] = ""
line k (x:[]) = (head (head (transpose (x:[])))) ++ fill k ++ "|" ++ (line k (transpose (tail (transpose (x:[])))))
            where fill k = (replicate ((head (kolom (findHead (head (head (transpose (x:[])))) k))) - (length (head (head (transpose (x:[])))))) ' ')


line k (x:xs) = (head (head (transpose (x:xs)))) ++ fill k ++ "|" ++ (line k (transpose (tail (transpose (x:xs)))))
            where fill k = (replicate ((head (kolom (head (transpose (x:xs))))) - (length (head (head (transpose (x:[])))))) ' ')






































project :: [String] -> Table -> Table
project [] [] = []
project [] (x:xs) = []
project s (x:xs) = transpose (project0 s (x:xs))

project0 :: [String] -> Table -> Table
project0 [] [] = []
project0 [] (x:xs) = []
project0 s (x:xs) = ([findHead (head s) (x:xs)] ++ (project0 (tail s) (x:xs)))










findString :: String -> [String] -> [String] -> [String]
findString s c [] = [""]
findString s c (x:[])|eqString s (head (x:[])) = c
                     |otherwise = []
findString s c (x:xs)|eqString s (head (x:xs)) = c
                     |otherwise = findString s c (tail (x:xs))







findHead :: String -> Table -> [String]
findHead s [] = ["Not Found"]
findHead s (x:[])|eqString s (head (head (transpose (x:[])))) = head (transpose (x:[]))
                 |otherwise = []
findHead s (x:xs)|eqString s (head (head (transpose (x:xs)))) = head (transpose (x:xs))
                 |otherwise = findHead s (transpose (tail (transpose (x:xs))))












select :: String -> (String -> Bool) -> Table -> Table
select "" b (_:_) = [["Not Found"]]
select s b (x:xs) = [head (x:xs)] ++ make (check s b (x:xs)) (xs)










make :: [Bool] -> Table -> Table
make [][[]] = [[]]
make (x:[]) (y:ys)| eqBool x True = y:[]
                  | otherwise = []
make (x:xs) (y:[])| eqBool x True = y:[]
                  | otherwise = []
make (x:[]) (y:[])| eqBool x True = y:[]
                  | otherwise = []
make (x:xs) (y:ys)| eqBool x True = y : (make (xs)(ys))
                  | otherwise = make (xs)(ys)

check :: String -> (String -> Bool) -> Table -> [Bool]

check s b (x:xs) = (map b (tail (findHead s (x:xs))))





















join :: Table -> Table -> Table
join [][] = []




match :: Table -> Table -> String
match [][] = []



match (x:[])(y:ys)|eqString  (head (map head (transpose x:[]))) (flat (findHead (head (map head (transpose x:[]))) (transpose (y:ys)))) = head (map head (transpose x:[]))
                  |otherwise = ["Empty"]


match (x:xs)(y:ys)|eqString  (head (map head (transpose x:xs))) (flat (findHead (head (map head (transpose x:xs))) (transpose (y:ys)))) = head (map head (transpose x:xs))
                  |otherwise = match (xs)(y:ys)

-- should delete []
-- 239,33-39
