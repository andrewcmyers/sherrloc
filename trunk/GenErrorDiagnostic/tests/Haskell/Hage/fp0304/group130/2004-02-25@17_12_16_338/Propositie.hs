module Propositie where

import Data.List

data Prop
     = En [Prop]
     | Of [Prop]
     | Niet Prop
     | Prop :-> Prop
     | Var String
     | Bool Bool

type Bedeling = [String]

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
evalueer (Bool x) _ = x
evalueer (Var x) b  = elemBy eqString x b
evalueer (Niet x) b = not( evalueer x b )
evalueer (En x) b   = and(map((flip evalueer) b) x)
evalueer (Of x) b   = or(map((flip evalueer) b) x)
evalueer (x:->y) b  = evalueer (Of [Niet x,y]) b



variablensub :: Prop -> [String]
variablensub (Var x)    = [x]
variablensub (Bool b)   = []
variablensub (Niet x)   = variablensub x
variablensub (x:->y)    = (variablensub x) ++ (variablensub y)
variablensub (En (x:xs))= (variablensub x) ++ (variablensub (En xs))
variablensub (En [])    = []
variablensub (Of (x:xs))= (variablensub x) ++ (variablensub (Of xs))
variablensub (Of [])    = []

variablen :: Prop -> [String]
variablen p = nubBy (eqList eqChar) (variablensub p)

subs []       = [ [] ]
subs (x:xs)   = map (x:) subsxs ++ subsxs
     where subsxs = subs xs

vervulbaar :: Prop -> [Bedeling]
vervulbaar p = filter (evalueer p) (subs(variablen p))



tautologie :: Prop -> Bool
tautologie p = and(map (evalueer p) (subs (variablen p)))



contradictie :: Prop -> Bool
contradictie p = null( vervulbaar p )



toon :: Prop -> String
toon (Niet p)   = "!"++(toon p)
toon (Var x)    = x
toon (Bool x)   | x = "True"
                | otherwise = "False"
toon (x:->y)    = "(" ++ toon x ++ " -> " ++ toon y ++ ")"
toon (En (p:ps))= "(" ++ (toon p) ++ " /\\ " ++ (concatMap toon (En ps)) ++ ")"
toon (Of (p:ps))= "(" ++ (toon p) ++ " \\/ " ++ (concatMap toon ps) ++ ")"




alleVars :: Prop -> Prop -> [String]
alleVars p q = nubBy (eqList eqChar) (variablen p ++ variablen q)

equivalentSub :: Prop -> Prop -> [[String]] -> Bool
equivalentSub p q (x:xs) | (eqBool (evalueer p x) (evalueer q x)) = (equivalentSub p q xs)
                         | otherwise = False
equivalentSub p q []     = True

equivalent :: Prop -> Prop -> Bool
equivalent p q = equivalentSub p q (subs (alleVars p q))



deMorgan :: Prop -> Prop
deMorgan (Bool x) = Bool x
deMorgan (Var x) = Var x
deMorgan (Niet (En ps)) = Of (map omkeer ps)
deMorgan (Niet (Of ps)) = En (map omkeer ps)
deMorgan (Niet x)       = Niet (deMorgan x)
deMorgan (En ps) = En (map deMorgan ps)
deMorgan (Of ps) = Of (map deMorgan ps)
deMorgan (x :-> y) = (deMorgan x) :-> (deMorgan y)

omkeer :: Prop -> Prop
omkeer p = deMorgan (Niet p)

-- (En ps) should be ps
-- 73,66-70
