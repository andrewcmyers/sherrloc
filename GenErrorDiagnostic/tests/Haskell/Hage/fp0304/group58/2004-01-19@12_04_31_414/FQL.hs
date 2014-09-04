module FQL where

import Data.List

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined

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








columnWidth :: Table -> [Int]
columnWidth a = map (\x -> foldr max 0 (map length x)) (transpose a)



paddStr :: Int -> Char -> String -> String
paddStr w c s = s ++ replicate (w-(length s)) c




makeLine :: [Int] -> [String] -> String
makeLine (w1:wt) (str1:strt) = "|" ++ paddStr w1 ' ' str1 ++ makeLine wt strt
makeLine _ _                 = "|\n"


makeBorder :: [Int] -> String
makeBorder a = foldr (\x y -> '+' : (paddStr x '-' "") ++ y) "+\n" a


writeTable :: Table -> String
writeTable [] = "\n"
writeTable (t1:t2) = makeBorder w ++ makeLine w t1 ++ makeBorder w ++ (concatMap (makeLine w) t2) ++ makeBorder w
 where w = columnWidth(t1:t2)




project :: [String] -> Table -> Table
project s t = transpose (concatMap (\x -> filter (\y -> eqString x (head y)) (transpose t)) s)




indexFrom :: [String] -> (String -> Bool) -> Int -> [Int]
indexFrom [] _ _ = []
indexFrom (s:ss) f i | s       = i : indexFrom ss f (i+1)
                     | otherwise = indexFrom ss f (i+1)


select :: String -> (String -> Bool) -> Table -> Table
select s f t = (t !! 0) : map (t !!) (indexFrom (head (transpose (tail (project [s] t)))) f 1)




common :: Table -> Table -> [String]
common a b = filter (\x -> elemBy eqString x (head a)) (head b)


langste :: Table -> Table -> Table
langste a b | (length a) > (length b) = a
            | otherwise               = b


join :: Table -> Table -> Table
join a b | (length cm) /= 1 = error "Geen of meerdere overeenkomende kolommen!"
         | otherwise        = (project cm (langste a b))
 where cm = common a b

-- s should be: f s
-- 86,24-24
