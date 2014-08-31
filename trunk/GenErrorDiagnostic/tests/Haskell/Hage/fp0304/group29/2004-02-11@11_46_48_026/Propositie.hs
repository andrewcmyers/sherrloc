module Propositie where

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

type Bedeling = [String]

eqString      :: String -> String -> Bool 
eqString = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqChar      :: Char -> Char -> Bool
eqChar = undefined


getVar :: String -> Bedeling -> Prop
getVar _ [] = Bool False
getVar x (y:ys) | x `eqString` y = Bool True
                | otherwise = getVar x ys


evalueer :: Prop -> Bedeling -> Bool
evalueer x bed = toBool (losOp x bed)



losOp :: Prop -> Bedeling -> Prop
losOp (Bool a) _ = Bool a
losOp (Var a) bed = losOp (getVar a bed) bed
losOp (Niet a) bed  = Bool (not uitkomst)
                    where uitkomst = evalueer a bed
losOp (Of x) bed = toProp uitkomst
                 where uitkomst = foldr (||) False (map (evaluate bed) x)
                       evaluate a b = evalueer b a
losOp (En x) bed = toProp uitkomst
                 where uitkomst = foldr (&&) True (map (evaluate bed) x)
                       evaluate a b = evalueer b a
losOp (a :-> b) bed = losOp (Of [(Niet a),b]) bed



toProp :: Bool -> Prop
toProp x | x = Bool True
         | otherwise = Bool False




toBool :: Prop -> Bool
toBool (Bool a) = a
toBool _ = False



getVars :: Prop -> [String]
getVars (Bool _) = []
getVars (Var x) = x:[]
getVars (En x) = foldr (++) [] (map getVars x)
getVars (Of x) = foldr (++) [] (map getVars x)
getVars (a :-> b) = (getVars a)++(getVars b)
getVars (Niet x) = getVars x



remDouble :: [String] -> [String]
remDouble [] = []
remDouble (x:xs) | x `isIn` xs = remDouble xs
                 | otherwise = x:(remDouble xs)



isIn :: String -> [String] -> Bool
isIn _ [] = False
isIn x (y:ys) | x `eqString` y = True
              | otherwise = isIn x ys



subRows :: [a] -> [[a]]
subRows [] = [[]]
subRows (x:xs) = map (x:) subs ++ subs
               where subs = subRows xs




vervulbaar :: Prop -> [Bedeling]
vervulbaar pro = [ x | x<-deelrijen, (evalueer pro x) ]
               where vars = remDouble (getVars pro)
                     deelrijen = subRows vars




tautologie :: Prop -> Bool
tautologie pro = (length x)==(length y)
               where x=subRows (remDouble (getVars pro))
                     y=vervulbaar pro



contradictie :: Prop -> Bool
contradictie pro = (length (vervulbaar pro))==0



toonDraft :: Prop -> String
toonDraft (Bool True) = "True"
toonDraft (Bool False) = "False"
toonDraft (Var x) = x
toonDraft (Niet x) = "!"++(toonDraft x)
toonDraft (En []) = []
toonDraft (En (x:xs)) = "("++(foldl op (toonDraft x) (map toonDraft xs))++")"
                      where op a b = a ++ " /\\ " ++ b
toonDraft (Of []) = []
toonDraft (Of (x:xs)) = "("++(foldl op (toonDraft x) (map toonDraft xs))++")"
                      where op a b = a ++ " \\/ " ++ b
toonDraft (a :-> b) = "("++(toonDraft a)++" -> "++(toonDraft b)++")"




equivalent :: Prop -> Prop -> Bool
equivalent pro1 pro2 = isGelijk vervul1 vervul2
                     where vervul1 = [ x | x<-joinedVars, (evalueer pro1 x)]
                           vervul2 = [ y | y<-joinedVars, (evalueer pro2 y)]
                           joinedVars = subRows (remDouble ((getVars pro1)++(getVars pro2)))






isGelijk :: [Bedeling] -> [Bedeling] -> Bool
isGelijk bed1 bed2 = eqList (eqList (eqString)) bed1 bed2


deMorgan :: Prop -> Prop
deMorgan (Niet(En [a,b])) = Of [deMorgan (Niet a), deMorgan (Niet b)]
deMorgan (Niet(Of [a,b])) = En [deMorgan (Niet a), deMorgan (Niet b)]
deMorgan (Var a) = (Var a)
deMorgan (Bool a) = (Bool a)
deMorgan (En a) = En (map deMorgan a)
deMorgan (Of a) = Of (map deMorgan a)
deMorgan (a :-> b) = (deMorgan a) :-> (deMorgan b)
deMorgan (Niet (Niet a)) = deMorgan a
deMorgan (Niet a) = Niet (deMorgan a)


telHaakjes :: String -> Int
telHaakjes [] = 0
telHaakjes (x:xs) | (x `eqChar` '(') = 1 + telHaakjes xs
                  | otherwise = telHaakjes xs


findChar :: Char -> String -> Int
findChar _ [] = 0
findChar f (x:xs) | f `eqChar` x = 0
                  | otherwise = 1 + findChar f xs


findCharReverse :: Char -> String -> Int
findCharReverse f x = (length x)-(findChar f (reverse x)) - 1


findNth :: (Char->String->Int) -> Char -> String -> Int -> Int
findNth find f x 0 = find f x
findNth find f x n = k + findNth find f (drop k x) (n-1)
                   where k = (find f x) + 1


getIndeces :: String -> Int -> (Int,Int)
getIndeces [] _ = (0,0)
getIndeces x n = (left,right)
               where left = findNth findChar '(' x n
                     right = findNth findChar ')' x ((telHaakjes n)-n)


getBetween :: String -> (Int,Int) -> String
getBetween [] _ = []
getBetween x (b,e) = take (e-b-1) (drop (b+1) x)


dropFromTo :: String -> (Int,Int) -> String
dropFromTo [] _ = []
dropFromTo x (b,e) = (take b x)++(drop (e+1) x)


getParts :: String -> [String]
getParts [] = []
getParts x = k : getParts y
           where k = getBetween hx index
                 index = getIndeces hx ((telHaakjes hx)-1)
                 y = dropFromTo hx index
                 hx = heeftHaakjes x


heeftHaakjes :: String -> String
heeftHaakjes x | (((head x) `eqChar` '(') && ((last x) `eqChar` ')')) = x
               | otherwise = "("++x++")"


getPriorities :: Prop -> [Int]
getPriorities (Var _) = [0]
getPriorities (Bool _) = [0]
getPriorities (x :-> y) = [4]++(getPriorities x)++(getPriorities y)
getPriorities (Of x) = [3]++(concatMap getPriorities x)
getPriorities (En x) = [2]++(concatMap getPriorities x)
getPriorities (Niet x) = [1]++(getPriorities x)

-- n should be: x
-- 181,66-66
