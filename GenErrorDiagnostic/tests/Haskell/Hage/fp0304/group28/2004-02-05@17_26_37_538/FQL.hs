module FQL where

import Data.List

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

type Table = [[String]]
type Kolom = [String]

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

tableLength :: Table -> [Int]
tableLength a = map kolomLength (transpose a)
        where kolomLength b = maximum(map length b)

makeString :: String -> Char -> Char -> Int -> String
makeString a d c b = d:a++(replicate (b-length a) c)

writeTable :: Table -> String
writeTable [] = "lege tabel"
writeTable (a:as) = concat(addLine++(tablelize [a] (a:as))++addLine++(tablelize as (a:as))++addLine)
           where tablelize d e = concat(map (++["|\n"]) (transpose(makeTable (transpose d) (tableLength e))))
                 addLine = (map (makeString "-" '+' '-') (tableLength (a:as)))++["+\n"]


makeTable :: Table -> [Int] -> Table
makeTable [] [] = [[]]
makeTable (a:as) (b:bs) = [(makeKolom a b)]++(makeTable as bs)
          where makeKolom (c:cs) d = [(makeString c '|' ' ' d)]++(makeKolom cs d)
                makeKolom [] _ = []
makeTable [] (_:_) = [[]]
makeTable (_:_) [] = [[]]

count :: [String] -> String -> Int
count [] _ = 0
count (a:as) b | (eqString a b) = 0
                 | otherwise = 1+(count as b)

project :: [String] -> Table -> Table
project _ [] = [["lege tabel"]]
project a (b:bs) = transpose(map (pak (b:bs)) c)
                  where c = map (count b) a

pak :: Table -> Int -> [String]
pak [] _ = []
pak (b:bs) a = (b!!a):pak bs a

select :: String -> (String -> Bool) -> Table -> Table
select a f b | eqString a d = transpose([d:(filter f ds)])
             | otherwise = select a f cs
               where (c:cs) = transpose b
                     (d:ds) = c

join :: Table -> Table -> Table
join [] [] = []
join (a:as) (b:bs) = [x ++ (filter (not(eqString (gemeen2 x y))) y) | x<-(a:as), y<-(b:bs), eqBool True (gemeen x y)]


gemeen :: [String] -> [String] -> Bool
gemeen [] _ = False
gemeen (a:as) b | elemBy eqString a b = True
                | otherwise = gemeen as b

gemeen2 :: [String] -> [String] -> String
gemeen2 [] _ = []
gemeen2 (a:as) b | elemBy eqString a b = a
                | otherwise = gemeen2 as b

-- should delete "not"
-- 83,37-39   83,37-63   83,37-64
