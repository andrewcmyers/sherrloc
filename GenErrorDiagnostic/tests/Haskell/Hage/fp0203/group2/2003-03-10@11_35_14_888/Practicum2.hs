module Practicum2(module List, module Practicum2) where

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
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

evalueer :: Prop -> Bedeling -> Bool
evalueer (En proplijst) bedeling = and (  map (`evalueer` bedeling) proplijst )
evalueer (Of proplijst) bedeling = or (  map (`evalueer` bedeling) proplijst )
evalueer (Niet x) bedeling = not(evalueer x bedeling)
evalueer (x :-> y) bedeling = (not (evalueer x bedeling)) || (evalueer y bedeling)
evalueer (Var variabele) bedeling = vind variabele bedeling
evalueer (Bool boolean) _ = boolean

vind :: String -> Bedeling -> Bool
vind _ [] = False
vind string bedeling = elemBy eqString string bedeling


vervulbaar :: Prop -> [Bedeling]
vervulbaar propositie = filter (evalueer propositie) (berekenAlleBedelingen(pakAlleVariabelen propositie))

pakAlleVariabelen :: Prop -> [String]
pakAlleVariabelen (Niet p) = pakAlleVariabelen p
pakAlleVariabelen (p1 :-> p2) = pakAlleVariabelen p1 ++ pakAlleVariabelen p2
pakAlleVariabelen (En (lijst)) = concat (map pakAlleVariabelen lijst)
pakAlleVariabelen (Of (lijst)) = concat (map pakAlleVariabelen lijst)
pakAlleVariabelen (Bool _) = []
pakAlleVariabelen (Var s) = [s]

berekenAlleBedelingen :: [String] -> [Bedeling]
berekenAlleBedelingen [] = [[]]
berekenAlleBedelingen (x:xs) = (berekenAlleBedelingen xs) ++ map (x:) (berekenAlleBedelingen xs)


tautologie :: Prop -> Bool
tautologie propositie = all (evalueer propositie) (berekenAlleBedelingen(pakAlleVariabelen propositie))


contradictie :: Prop -> Bool
contradictie propositie = all(not.evalueer propositie) (berekenAlleBedelingen(pakAlleVariabelen propositie))


toon  :: Prop -> String
toon (Var x)  = x
toon (En [x]) = toon x ++ ""
toon (En [x,y]) = "(" ++ toon x ++ " /\\ " ++ toon y ++ ")"
toon (En (x:xs)) = toon x ++ " /\\ " ++ toon (En (xs))
toon ( En []) = ""
toon (Bool x) | x   = "True"
               | otherwise = "False"
toon(Of [x]) = toon x ++ ""
toon (Of [x, y]) = "(" ++ toon x ++ " \\/ " ++ toon y ++ ")"
toon(Of (x:xs)) = toon x ++ " \\/ " ++ toon (Of xs)
toon (Of [])    = ""
toon (x :-> y) = toon x ++ " -> " ++ toon y
toon (Niet p)  = "!" ++ toon p


equivalent :: Prop -> Prop -> Bool
equivalent p1 p2 = eqList eqBool ((map (evalueer p1)) bedelingenLijst) ((map (evalueer p2)) bedelingenLijst)
           where bedelingenLijst = berekenAlleBedelingen (pakAlleVariabelen p1 ++ pakAlleVariabelen p2)



deMorgan :: Prop -> Prop
deMorgan (Var x) = Var x
deMorgan (Niet p) = Niet p
deMorgan (p1 :-> p2) = p1 :-> p2
deMorgan (Bool b) = Bool b
deMorgan (Of proplijst) = (Of proplijst)
deMorgan (En proplijst) = (En proplijst)
deMorgan (Niet (En proplijst))= Of(Niet proplijst)

-- Niet proplijst should be: map Niet proplijst
-- 89,36-49
