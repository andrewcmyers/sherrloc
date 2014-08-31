module Prac2 where
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
eqChar      :: Char -> Char -> Bool
eqChar = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

evalueer :: Prop -> Bedeling -> Bool

evalueer (Var x) bed | elemBy eqString x bed = True
                     |otherwise = False
evalueer (Bool b) _ = b

evalueer (En []) _ = False
evalueer (En (x:xs)) bed = and (map (\prop -> evalueer prop bed) (x:xs))

evalueer (Of []) _ = False
evalueer (Of (x:xs)) bed = or (map (\prop -> evalueer prop bed) (x:xs))

evalueer (Niet prop) bed = not (evalueer prop bed)

evalueer (prop1 :-> prop2) bed |evalueer prop1 bed && evalueer (Niet(prop2)) bed = False
                               |otherwise = True





vervulbaar :: Prop -> [Bedeling]
vervulbaar prop = filter (evalueer prop) (test prop)

test :: Prop -> [Bedeling]
test prop = powerset (geendubbel(variLijst prop))


variLijst :: Prop -> Bedeling
variLijst (Var x)            = [x]

variLijst (Bool b)           |b = [[]]
                             |otherwise =[[]]

variLijst (En prop) = concatMap variLijst prop

variLijst (Of prop) = concatMap variLijst prop

variLijst (Niet prop)        = variLijst prop

variLijst (prop1 :-> prop2)  = variLijst prop1 ++ variLijst prop2


powerset :: Bedeling -> [Bedeling]
powerset [] = [[]]
powerset (x:xs) = (powerset xs) ++ map(x:) (powerset xs)


geendubbel :: Bedeling -> Bedeling
geendubbel [] = []
geendubbel (x:xs) | (elemBy eqString x xs) = geendubbel xs
                  | otherwise = x : geendubbel xs


tautologie :: Prop -> Bool
tautologie prop = eqList eqBed (vervulbaar prop) (test prop)


contradictie :: Prop -> Bool
contradictie prop = not (tautologie prop)



eqBed :: Bedeling -> Bedeling -> Bool
eqBed bed1 bed2 = eqList eqString bed1 bed2




toon :: Prop -> String
toon (Var x) = x

toon (Bool y) |y = "True"
              |otherwise = "False"

toon (En props)        | and(map simpelEn props) = concat(intersperse " /\\ " (map toon props))
                       | head (map simpelEn props) = toon (head props) ++ " /\\ ("  ++ toon (last props) ++ ")"
                       | last (map simpelEn props) = "(" ++ toon (head props) ++ ") /\\ " ++ toon (last props)
                       | otherwise = "(" ++ (concat(intersperse ") /\\ (" (map toon props))) ++ ")"

toon (Of props)        | and(map simpel props) = concat(intersperse " \\/ " (map toon props))
                       | head (map simpel props) = toon (head props) ++ " \\/ ("  ++ toon (last props) ++ ")"
                       | last (map simpel props) = "(" ++ toon (head props) ++ ") \\/ " ++ toon (last props)
                       | otherwise = "(" ++ (concat(intersperse ") \\/ (" (map toon props))) ++ ")"

toon (Niet prop) |simpel prop = "!" ++ toon prop
                 |otherwise = "!(" ++ toon prop ++ ")"

toon (prop1 :-> props)   |simpel prop1 && simpel props = toon prop1 ++ " -> " ++ toon props
                         |simpel prop1 = toon prop1 ++ " -> (" ++ toon props ++ ")"
                         |simpel props = "(" ++ toon prop1 ++ ") -> " ++ toon props
                         |otherwise = "(" ++ toon prop1 ++ ") -> (" ++ toon props ++ ")"


simpel :: Prop -> Bool
simpel (Var _ ) = True
simpel (Bool _) = True
simpel (Niet _) = True
simpel _ = False



simpelEn :: Prop -> Bool
simpelEn (Var _ ) = True
simpelEn (Bool _) = True
simpelEn (Niet _) = True
simpelEn (En _) = True
simpelEn _ = False






equivalent :: Prop -> Prop -> Bool
equivalent (Bool b) (Bool c) = eqBool b c
equivalent (Bool b) p | b = tautologie p
                      | otherwise = contradictie p

equivalent p (Bool b) | b = tautologie p
                      | otherwise = contradictie p

equivalent p1 p2 =  eqList eqBed (vervulbaar p1) (vervulbaar p2)


deMorgan :: Prop -> Prop
deMorgan (Var x) = Var x
deMorgan (Bool x) = Bool x
deMorgan (En props) = En (map deMorgan props)
deMorgan (Of props) = Of (map deMorgan props)
deMorgan (Niet(En props)) = Of(map deMorgan(map Niet(map deMorgan props)))
deMorgan (Niet(Of props)) = En(map deMorgan(map Niet(map deMorgan props)))
deMorgan (Niet(prop)) = Niet(deMorgan prop)
deMorgan (prop1 :-> prop2) = (deMorgan prop1) :-> (deMorgan prop2)

ontleed :: String -> Prop

ontleed s = (map fun(words s))
fun :: String -> String
fun ding | eqChar (head ding) '!' =  "Niet (" ++ (fun (tail ding)) ++ ")"
         | eqString ding ("->") = ding
         | eqString ding ("\\/") = ding
         | eqString ding ("/\\") = ding
         | eqString ding ("True") || eqString ding ("False") = "Bool" ++ ding
         |otherwise = "Var" ++ ding


-- signature should be: String -> [String]
-- 160,12-25
