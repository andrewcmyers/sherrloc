module Database where

import Data.List

type Table = [[String]]

eqString      :: String -> String -> Bool 
eqString = undefined

compilers :: Table
compilers =
  [
  ["Compiler", "Universiteit/bedrijf"],
  ["Helium", "Universiteit van Utrecht"],
  ["NHC", "University of York"],
  ["GHC", "Microsoft Research"],
  ["Hugs", "Galois Connections"],
  ["Hugs.NET", "Galois Connections"],
  ["O'Haskell", "Oregon Graduate Institute"],
  ["O'Haskell", "Chalmers University of Technology"],
  ["HBC", "Chalmers University of Technology"]
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




colomWidths :: Table -> [Int]
colomWidths table = map maxLength t
            where t = transpose table
                  maxLength :: [String] -> Int
                  maxLength xs = maximum (map length xs)




writeline :: [Int] -> String
writeline cw = '+' : concatMap writeFirstLine cw
          where writeFirstLine :: Int -> String
                writeFirstLine 0 = "+"
                writeFirstLine n = '-' : writeFirstLine (n-1)




writerow :: [String] -> [Int] -> String
writerow xs ns = '|' : concat (zipWith writerowsimple xs ns)
         where writerowsimple :: String -> Int -> String
               writerowsimple x n = x ++ printspace (n - length x)
                              where printspace :: Int -> String
                                    printspace 0 = "|"
                                    printspace y = ' ' : printspace (y-1)




writeTitle :: Table -> String
writeTitle table = writerow (head table) (colomWidths table)




writeTable :: Table -> String
writeTable table = (writeline breedte) ++ "\n" ++ (writeTitle table)
                   ++ "\n" ++  writeline breedte ++ "\n" ++ concatMap f (tail table)
                   ++ writeline breedte ++ "\n"
                      where breedte = colomWidths table
                            f row = writerow row (colomWidths table) ++ "\n"






project :: [String] -> Table -> Table
project lijst table = transpose ( map ( f ( transpose table ) ) lijst )
                   where   f [] _ = []
                           f (x:xs) s | eqString ( head x ) s = x

                                      | otherwise= f xs s







select :: String -> (String -> Bool) -> Table -> Table
select string f table = head table : filter g (tail table)
                where g rij = f (rij !! n)
                      n = index string (head table)

index :: String -> [String] -> Int
index x xs | eqString x (head xs) = 0
           | otherwise            = 1 + index x (tail xs)












eqElement :: Int -> Int -> Bool
eqElement (x:xs) (y:xy) | x == y = True
                        | otherwise = eqElement xs xy

-- signature should be [Int] -> [Int] -> Bool
-- 118,14-16   118,21-23    118,14-16   118,21-23
