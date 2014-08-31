module Prac2C where

import Data.List

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

type Bedeling = [String]

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

evalueer:: Prop -> Bedeling -> Bool
evalueer (En rest) lijst = and (map f rest)
         where f pee = evalueer pee lijst
evalueer (Of rest) lijst = or (map f rest)
         where f pee = evalueer pee lijst
evalueer (Niet rest) lijst = not (evalueer rest lijst)
evalueer (p :-> q) lijst
         | ((evalueer p lijst) `eqBool` True) && ((evalueer q lijst) `eqBool` False) = False
         | otherwise = True
evalueer (Var x) lijst = elemBy eqString x lijst
evalueer (Bool y) [] = y
evalueer (Bool y) (x:xs) = y




vervulbaarheid:: Prop -> [Bedeling]
vervulbaarheid rest = filter (evalueer rest) (subs (wegDubbel (findVar rest)))

findVar:: Prop -> [String]
findVar (Bool y) = []
findVar (Var x) = [x]
findVar (Niet rest) = findVar rest
findVar (En rest) = concat (map findVar rest)
findVar (Of rest) = concat (map findVar rest)
findVar (rest1 :-> rest2) = (findVar rest1) ++ (findVar rest2)

wegDubbel:: [String] -> [String]
wegDubbel [] = []
wegDubbel (x:xs)
          | elemBy (eqString) x xs = wegDubbel xs
          | otherwise = x: wegDubbel xs

subs:: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
     where subsxs = subs xs




tautologie:: Prop -> Bool
tautologie rest = and (map (evalueer rest) (subs (wegDubbel (findVar rest))))




contradictie:: Prop -> Bool
contradictie rest = not (tautologie rest)




verw:: Prop -> String
verw (En rest) = "(" ++ invoegen (" /\\ ")(map verw rest) ++ ")"
verw (Of rest) = "(" ++ invoegen (" \\/ ")(map verw rest) ++ ")"
verw (rest1 :-> rest2) = "(" ++ verw rest1 ++ " -> " ++ verw rest2 ++ ")"

verw (Niet rest) = "!" ++ verw rest
verw (Var x) = x
verw (Bool y) = if y then "True" else "False"

invoegen:: String -> [String] -> String
invoegen (s) [x] = x
invoegen (s) (x:xs) = x ++ s ++ invoegen s xs




equivalent:: Prop -> Prop -> Bool
equivalent p1 p2 = eqList (eqBool) (map (evalueer p1) bla) (map (evalueer p2) bla)
           where bla = subs (nubBy eqString ((findVar p1)++(findVar p2)))



deM:: Prop ->  Prop
deM (Bool y) = (Bool y)
deM (Var x) = (Var x)
deM (Niet (En (xs))) = Of(map Niet xs)
deM (Niet (Of (xs))) = En(map Niet xs)
deM (En (xs)) = En (map deM xs)
deM (Of (xs)) = Of (map deM xs)
deM (Niet xs) = Niet (map deM xs)
deM (rest1 :-> rest2) = (deM rest1 :-> deM rest2)


voegPrefix:: String -> [String] -> String
voegPrefix (s) [x] = x
voegPrefix (s) (x:xs) = s ++ x ++ invoegen s xs





eqElemList _      x     []     =  False
eqElemList eqElem x (y:ys)
           | x `eqElem` (concat y) = True
           | otherwise = eqElemList eqElem x ys
eqElemList _      _      _      = False














proplijst1 = (En [(Var "q" :-> Var "p"), Var "p",Of[Var "z", Var "q"]])
proplijst2 = (En [(Var "q" :-> Var "p"), Var "p",Of[Var "q", Var "z"]])
proplijst3 = (En [(Var "rq" :-> Var "p"), Var "p",Of[Var "qv", Var "z"]])



















filter2 p xs = [k | k <- xs, (not . p) k]

-- should delete "map"
-- 106,23-25   106,23-32
