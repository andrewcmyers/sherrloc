module Prac1 where

import Data.List

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined

type Table = [[String]]



adslOplossingen  ::  Table
adslOplossingen =
        [ ["Abonnement", "Provider", "EUR"]
        , ["BelBudget", "KPN", "9,47"]
        , ["FastWeb Diamond", "TMF", "69,95"]
        , ["FastWeb Platinum", "TMF", "54,95"]
        ]

compilers  ::  Table
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

locaties  ::  Table
locaties =
  [ ["Universiteit/bedrijf", "Land", "Stad"]
  , ["Universiteit van Utrecht", "Nederland", "Utrecht"]
  , ["University of York", "Engeland", "York"]
  , ["Microsoft Research", "Engeland", "Cambridge"]
  , ["Galois Connections", "Verenigde Staten", "Beaverton"]
  , ["Oregon Graduate Institute", "Verenigde Staten", "Beaverton"]
  , ["Chalmers University of Technology", "Zweden", "Goteborg"]
  ]







writeTable  ::  Table -> String



kolomBreedte  ::  Table -> [Int]
streepPlus  ::  [Int] -> String
barData  ::  [Int] -> [String] -> String

kolomBreedte t = map maximum (transpose (map (map length) t))

streepPlus kb = "+" ++ concatMap (\n -> (replicate n '-') ++ "+") kb ++ "\n"

barData kb s = "|" ++ concat(zipWith dat kb s) ++ "\n"
      where dat l t = t ++ replicate (l-(length t)) ' ' ++ "|"




op1  ::  Table -> IO ()
op1 a = putStr (writeTable a)

writeTable [] = []
writeTable (x:xs) = sp ++ barData kb x ++ sp ++ foldr ((++).(barData kb)) "" xs ++ sp
     where kb = kolomBreedte (x:xs)
           sp = streepPlus kb








project  ::  [String] -> Table -> Table



kolomFilter :: String -> Table -> Table
kolomFilter _ [] = []
kolomFilter s (x:xt) | eqString s (head x) = x : kolomFilter s xt
                     | otherwise = kolomFilter s xt

tabelFilter :: [String] -> Table -> Table
tabelFilter [] _ = []
tabelFilter (x:xs) t = kolomFilter x (transpose t) ++ tabelFilter xs t




op2  ::  [String] -> Table -> IO ()
op2 s t = putStr (writeTable(project s t))

project s t = transpose (tabelFilter s t)









select  ::  String -> (String -> Bool) -> Table -> Table



vergelijk :: String -> (String -> Bool) -> Table -> [Bool]
vergelijk s f t = map f (head(transpose(project [s] t)))

filterRec :: [Bool] -> [[String]] -> [[String]]
filterRec [] (_:_) = []
filterRec (_:_) [] = []
filterRec [] [] = []
filterRec (b:bs) (sl:sls) | b = sl : filterRec bs sls
                          | otherwise = filterRec bs sls




op3  ::  String -> (String -> Bool) -> Table -> IO ()
op3 s f t = putStr (writeTable (filterRec (True : (tail (vergelijk s f t))) t))

select s f t = filterRec (True : (tail(vergelijk s f t))) t









join  ::  Table -> Table -> Table



carthesianProduct :: [[a]] -> [[a]] -> [[a]]
carthesianProduct a b = transpose ((transpose expandA) ++ (transpose expandB))
     where expandA = concat (map (replicate (length b)) a)
           expandB = concat (replicate (length a) b)

findEquals :: (a -> a -> Bool) -> [a] -> (Int, Int)
findEquals eqMethod l = (firstEqual eqMethod l 0, ((length l) - (firstEqual eqMethod (reverse l) 1)))
      where firstEqual :: (a -> a -> Bool) -> [a] -> Int -> Int
            firstEqual _ [] _ = error "No equal members in list found"
            firstEqual eqM (v:vs) n | elemBy eqM v vs = n
                                    | otherwise       = firstEqual eqM vs (n+1)
filterNotJoined :: Table -> Table
filterNotJoined t = filter joined t
     where joined a = eqString (a !! (fst jK)) (a !! (snd jK))
           jK = findEquals eqString (head t)

filterDoubleRow :: Table -> Table
filterDoubleRow t = transpose (removeIndex (transpose t) removepos)
      where removepos = snd (findEquals eqString (head t))

removeIndex :: [a] -> Int -> [a]
removeIndex t i = fst splitted ++ snd (splitAt 1 (snd splitted))
      where splitted = splitAt i t





op4  ::  Table -> Table -> IO ()
op4 a b = putStr (writeTable join a b)

join a b = filterDoubleRow (filterNotJoined (combined a b))
        where combined (ka:sa) (kb:sb) = carthesianProduct [ka] [kb] ++ carthesianProduct sa sb

-- missing () around join a b
-- 177,19-37   177,19-28
