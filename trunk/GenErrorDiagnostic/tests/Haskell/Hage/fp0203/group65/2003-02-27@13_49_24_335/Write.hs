module Write(module List, module Write) where

import Data.List hiding (insert)

eqString      :: String -> String -> Bool 
eqString = undefined
eqInt      :: Int -> Int -> Bool 
eqInt = undefined


insert                  ::  (a->a->Ordering) -> a -> [a] -> [a]
insert cmp e []         =  [e]
insert cmp e (x:xs)     =  case e `cmp` x of
                           LT -> e : x : xs
                           _ -> x : insert cmp e xs

isort                   ::  (a->a->Ordering) -> [a] -> [a]
isort cmp = foldr (insert cmp) []




type Table = [[String]]

compilers :: Table
compilers = [["Compiler", "Universiteit/bedrijf"]
            ,["Helium", "Universiteit van Utrecht"]
            ,["NHC", "University of York"]
            ,["GHC", "Microsoft Research"]
            ,["Hugs", "Galois Connections"]
            ,["Hugs.NET", "Galois Connections"]
            ,["O'Haskell", "Oregon Graduate Institute"]
            ,["O'Haskell", "Chalmers University of Technology"]
            ,["HBC", "Chalmers University of Technology"]
            ]

locaties :: Table
locaties = [["Universiteit/bedrijf", "Land", "Stad"]
           ,["Universiteit van Utrecht", "Nederland", "Utrecht"]
           ,["University of York", "Engeland", "York"]
           ,["Microsoft Research", "Engeland", "Cambridge"]
           ,["Galois Connections", "Verenigde Staten", "Beaverton"]
           ,["Oregon Graduate Institute", "Verenigde Staten", "Beaverton"]
           ,["Chalmers University of Technology", "Zweden", "Goteborg"]
           ]







lijstLengteConversie :: [String] -> [Int]
lijstLengteConversie [] = []
lijstLengteConversie (x:xs) = length x : lijstLengteConversie (xs)

tableLengteConversie :: Table -> [[Int]]
tableLengteConversie [] = []
tableLengteConversie (x:xs) = (lijstLengteConversie x) : (tableLengteConversie xs)


transTableConv :: Table -> [[Int]]
transTableConv [] = []
transTableConv (x:xs) = transpose (tableLengteConversie (x:xs))

grootste :: [Int] -> Int
grootste [] = 0
grootste xs = 1

tableLengteLijst :: Table -> [Int]
tableLengteLijst [] = []
tableLengteLijst tabel = map grootste (transTableConv tabel)



randen :: [Int] -> String
randen [] = "+"
randen (x:xs) = ("+" ++ (replicate x '-')) ++ randen(xs)

regel :: [String] -> Table -> String
regel [] a = "|\n"
regel (x:xs) a = "|" ++ x ++ (replicate (((tableLengteLijst a)!!(length (head a) - (length xs) - 1))-length (x)) ' ') ++ (regel (xs) a)



multiRegel :: [[String]] -> Table -> String
multiRegel [] a = randen (tableLengteLijst a)
multiRegel (x:xs) a = (regel (x) a) ++ (multiRegel (xs) a)

writeTable :: Table -> String
writeTable [] = putStr " "
writeTable (x:xs) = ((randen (tableLengteLijst(x:xs))) ++ "\n"  ++ (regel x (x:xs)) ++ (randen (tableLengteLijst(x:xs))) ++ "\n" ++ (multiRegel xs (x:xs)) ++ "\n")



selecteerKolomNummerRechtsAf :: String -> Table-> Int
selecteerKolomNummerRechtsAf a [] = (-1)
selecteerKolomNummerRechtsAf a (x:xs) = case (eqString a (head x)) of
                                            True -> length (xs)
                                            False -> selecteerKolomNummerRechtsAf a (xs)

selecteerKolomNummerLinks :: String -> Table -> Int
selecteerKolomNummerLinks a [] = (-1)
selecteerKolomNummerLinks a b = case (eqInt (selecteerKolomNummerRechtsAf a b) (-1)) of
                                     True -> (-1)
                                     False -> (length b) - (selecteerKolomNummerRechtsAf a b)

selectTransLinks :: String -> Table -> Int
selectTransLinks a b = selecteerKolomNummerLinks a (transpose b)

projTrans :: [String] -> Table -> Table
projTrans [] a = []
projTrans (x:xs) a = case (eqInt (selectTransLinks x a)(-1)) of
                          True -> projTrans (xs) a
                          False -> ((!!) (transpose(a)) ((selectTransLinks x a)-1)): (projTrans (xs) a)

project :: [String] -> Table -> Table
project a b = transpose (projTrans a b)










selRijOnd :: (String -> Bool) -> [String] -> [Int]
selRijOnd a [] = []
selRijOnd a (x:xs) = case (a x) of
                     True -> (length (xs)) :  (selRijOnd a (xs))
                     False -> selRijOnd a (xs)







bouwTabelKolomOnderAf :: (String -> Bool) -> [Int] -> Table -> Table
bouwTabelKolomOnderAf a b [] = []
bouwTabelKolomOnderAf a [] [] = []
bouwTabelKolomOnderAf a [] b  = []
bouwTabelKolomOnderAf a (y:ys)(x:xs) = case (eqInt y (length(xs))) of
                                True -> [x] ++ (bouwTabelKolomOnderAf a (ys)(xs))
                                False -> bouwTabelKolomOnderAf a (y:ys) (xs)

bouwEasy :: (String -> Bool) -> [String] -> Table -> Table
bouwEasy a b [] = [[]]
bouwEasy a [] b = [[]]
bouwEasy a [] [] = [[]]
bouwEasy a b c = (head c) : (bouwTabelKolomOnderAf a (selRijOnd a (tail b)) c)







projEnkel :: String -> Table -> [String]
projEnkel x a = case (eqInt (selectTransLinks x a)(-1)) of
                   True -> [[]]
                   False -> ((!!) (transpose(a)) ((selectTransLinks x a)-1))


select :: String -> (String -> Bool) -> Table -> Table
select keuze vergel tabel = bouwEasy vergel (projEnkel keuze tabel) tabel

-- putStr " " should be: " "
-- 91,17-26
