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

test :: String
test = writeTable (select "Universiteit/bedrijf" (eqString "Galois Connections") compilers)

writeTable :: Table -> String

writeTable tabel =  printplusjes maxs
                 ++ printlijn maxs (head tabel)
                 ++ printplusjes maxs
                 ++ concat (map (printlijn maxs) (tail tabel))
                 ++ printplusjes maxs
                 where maxs = berekenMaxima (transpose tabel)

berekenMaxima :: Table -> [Int]
berekenMaxima = map (lengteLangste)
              where lengteLangste x = maximum (map length x)

printplusjes :: [Int] -> String
printplusjes [] = "+\n"
printplusjes (x:xs) = '+':(streepjes x) ++ printplusjes xs
                    where streepjes s = replicate s '-'

printlijn :: [Int] -> [String] -> String
printlijn x y = concat (doubleMap fixString x y) ++ "|\n"
                   where fixString a b | (length b) >= a = '|':b
                                       | otherwise = '|':(b ++ replicate (a-length b) ' ')

doubleMap :: (a -> b -> c) -> [a] -> [b] -> [c]
doubleMap _ [] _ = []
doubleMap _ _ [] = []
doubleMap f (x:xs) (y:ys) = (f x y):(doubleMap f xs ys)

project :: [String] -> Table -> Table
project lijst tabel = transpose (map (getRowEq (transpose tabel)) lijst)
                 where getRowEq [] _= []
                       getRowEq (b:bs) a | eqString (head b) a = b
                                         | otherwise = getRowEq bs a

select :: String -> (String -> Bool) -> Table -> Table
select x f y = head y:filter (filterFun f (fieldno x (head y))) (tail y)
             where fieldno _ [] = 0
                   fieldno s (t:tb) | eqString s t = 0
                                    | otherwise = 1 + fieldno s tb

filterFun :: (String -> Bool) -> Int -> [String] -> Bool
filterFun f y s = f (getField y s)
                where getField num ls = head(drop num ls)

commonField :: Table -> Table -> String
commonField [] _ = ""
commonField _ [] = ""
commonField (x:_) (y:_) = head [a | a <- x, b <- y, eqString a b]

combine :: Table -> Table -> Table
combine _ [] = []
combine [] _ = []
combine (x:xs) (y:ys) = [x ++ y] ++ combine xs ys

join2 :: Table -> Table -> Table
join2 _ [] = []
join2 [] _ = []
join2 x y = [mergeRows a b | a <- x, b <- y, eqField a b]
      where
           mergeRows a b = [a ++ b]
           eqField a b = True


join :: Table -> Table -> Table
join a b = combine (project (filterFields a (commonField a b)) a) b
     where
           filterFields x y = filter (neqString y) (head x)

           neqString s1 s2 = not(eqString s1 s2)

-- [a++b] should be: a++b
-- 97,29-34
