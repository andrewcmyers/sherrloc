module Database2 where

import Data.List

type Table = [[String]]

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined

columnWidth :: [String] -> Int
columnWidth strs = maximum (width strs)


width :: [String] -> [Int]
width [] = []
width (x:xs) = length x : width xs





dimTable :: Table -> [Int]
dimTable table = map columnWidth (transpose table)


writeTable :: Table -> String
writeTable []     = ""
writeTable (x:xs) = writeSpacer dim ++ writeLine x dim ++ writeSpacer dim ++ writeBody xs dim  ++ writeSpacer dim
                  where dim = dimTable (x:xs)


writeLine :: [String] -> [Int] -> String
writeLine [] []         = "|\n"
writeLine [] _          = "|\n"
writeLine _  []         = "|\n"
writeLine (x:xs) (i:is) = "|" ++ x ++ spaces ++ writeLine xs is
                        where spaces = (replicate (i - length x) ' ')


writeBody :: Table -> [Int] -> String
writeBody  []     _ = []
writeBody  (x:xs) dim = writeLine x dim ++ writeBody xs dim


writeSpacer :: [Int] -> String
writeSpacer []     = "+\n"
writeSpacer (i:is) = "+" ++ (replicate i '-') ++ writeSpacer is





project :: [String] -> Table -> Table
project strs table = transpose (map (project' (transpose table)) strs)


project' :: Table -> String -> [String]
project' []     _   = []
project' (x:xs) str | eqString str (head x) = x
                    | otherwise             = project' xs str





select :: String -> (String -> Bool) -> Table -> Table
select str cond table = (head table) : map ( (tail table) !! ) mask
                      where mask = makeMask str cond (transpose table)

makeMask :: String -> (String -> Bool) -> Table -> [Int]
makeMask _   _    []                                     = []
makeMask str cond (htab:ttab) | eqString str (head htab) = findIndices  cond (tail htab)
                              | otherwise                = makeMask str cond ttab




join :: Table -> Table -> Table
join tab1 tab2 =  [x ++ (tail y) | x <- tab1 ,  y <- tab2, eqString (x !! 1) (head y)]
               where incommon = inCommon (head tab1) (head tab2)
                     a        = findIndices (eqString incommon) tab1
                     b        = findIndices (eqString incommon) tab2


inCommon :: [String] -> [String] -> String
inCommon _            []                                    = []
inCommon []           _                                     = []
inCommon (str1:strs1) strs2 | elemBy (eqString) str1 strs2  = str1
                            | otherwise                     = inCommon strs1 strs2

killColumn :: Table -> Int -> Table
killColumn table i = transpose( (init (fst split)) ++ (snd split)  )
                   where split = splitAt (i+1) (transpose table)

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

-- tab1 should be (head tab1); tab2 should be (head tab2)
-- 83,65-68   84,65-68
