module FQL (module List, module FQL) where

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
writeTable = unlines.insertEmptyLines


insertEmptyLines :: Table -> [String]
insertEmptyLines table = emp : head(t') : emp : tail(t') ++ [emp]
                         where emp = foldr (\x-> (++) ("+" ++ replicate x '-')) "+" (columnLength table)
                               t' = lineOutTable table



columnLength :: Table -> [Int]
columnLength table = map (maximum.map length) (transpose table)



lineOutTable :: Table -> [String]
lineOutTable table = map (lineOutRow (columnLength table)) table


lineOutRow :: [Int] -> [String] -> String
lineOutRow ls row = concat (zipWith func ls row) ++ "|"

                    where func dig str = "|" ++ take dig (str ++ repeat ' ')



project :: [String] -> Table -> Table
project list table  = transpose (map ((transpose table) !!) (selectColumnNrs (head table) list))



selectColumnNrs :: [String] -> [String] -> [Int]
selectColumnNrs tableHead list = map (\x-> colno [x] tableHead) list



colno :: [String] -> [String] -> Int
colno list (th:ths) | or (map (eqString th) list) = 0
                    | otherwise                   = 1 + colno list ths





select :: String -> (String -> Bool) -> Table -> Table
select string test table = (head table) : (selectRows (colno (head table) string) test (tail table))





selectRows :: Int -> (String -> Bool) -> Table -> Table
selectRows number test table = filter (rowTest number test) table


rowTest :: Int -> (String -> Bool) -> [String] -> Bool
rowTest number test list = test (list !! number)





join :: Table -> Table -> Table
join left right  = map (\x-> take b x ++ (drop (b+1) x)) (selecteq left right a b)
                   where a = colno (head right) (head left)
                         b = colno (head left)  (head right) + length (head left)




selecteq :: Table -> Table -> Int -> Int -> Table
selecteq left right a b = filter (\x-> eqString (x!!a) (x!!b)) (crossProduct left right)



crossProduct :: Table -> Table -> Table
crossProduct (l:ls) (r:rs) = zipWith (++) (l:(concat (map (replicate y) ls))) (r:(concat (replicate x rs)))
                             where x = length ls
                                   y = length rs

-- missing []
-- 79,75-80
