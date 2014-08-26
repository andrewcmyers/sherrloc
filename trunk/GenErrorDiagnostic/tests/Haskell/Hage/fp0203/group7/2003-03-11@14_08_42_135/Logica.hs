module Logica (module List, module Logica) where
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
eqChar      :: Char -> Char -> Bool 
eqChar = undefined


evalueer :: Prop -> Bedeling -> Bool
evalueer (Var x) bed     = elemBy eqString x bed
evalueer (Bool bool) _   = bool
evalueer (En []) _       = True
evalueer (En (p:ps)) bed = evalueer p bed && evalueer (En ps) bed
evalueer (Of []) _       = False
evalueer (Of (p:ps)) bed = evalueer p bed || evalueer (Of ps) bed
evalueer (Niet prop) bed = not (evalueer prop bed)
evalueer (p1 :-> p2) bed = not (evalueer p1 bed) || evalueer p2 bed







alleVar :: Prop -> [String]
alleVar (Var var)   = [var]
alleVar (Bool _)    = []
alleVar (En [])     = []
alleVar (En (p:ps)) = alleVar p ++ alleVar (En ps)
alleVar (Of [])     = []
alleVar (Of (p:ps)) = alleVar p ++ alleVar (Of ps)
alleVar (Niet prop) = alleVar prop
alleVar (p1 :-> p2) = alleVar p1 ++ alleVar p2



checkDubbele :: [String] -> [String]
checkDubbele lijst = nubBy eqString lijst


alleBed :: [String] -> [Bedeling]
alleBed [] = [ [] ]
alleBed (x:xs) = map (x:) alleBedxs ++ alleBedxs
  where alleBedxs = alleBed xs


vervulbaar :: Prop -> [Bedeling]
vervulbaar prop = filter (evalueer prop) (alleBed (checkDubbele (alleVar prop)))







sorteer :: [Bedeling] -> [Bedeling]
sorteer = undefined



tautologie :: Prop -> Bool
tautologie prop | eqList (eqList eqString)
                         (sorteer (vervulbaar prop))
                         (sorteer (alleBed (checkDubbele (alleVar prop))))
                            = True
                | otherwise = False







contradictie :: Prop -> Bool
contradictie prop | eqList (eqList eqString) (vervulbaar prop) [] = True
                  | otherwise = False







toon :: Prop -> String
toon (Var string)      = string
toon (En [Var string]) = string
toon (En (p:ps))       = "(" ++ toon p ++ " /\\ " ++ toon (En ps) ++ ")"
toon (Of [Var string]) = string
toon (Of (p:ps))       = "(" ++ toon p ++ " \\/ " ++ toon (Of ps) ++ ")"
toon (Niet prop)       = " ! " ++ toon prop
toon (p1 :-> p2)       = "(" ++ toon p1 ++ " -> " ++ toon p2 ++ ")"

checkHaken :: String -> String
checkHaken (x:xs) = if eqChar "(" x
                       then xs
                       else (x:xs)



















equivalent :: Prop -> Prop -> Bool
equivalent p1 p2 = if tautologie (En [(p1 :-> p2) , (p2 :-> p1)] ) then True
                   else False








negatie :: [Prop] -> [Prop]
negatie props =  map (deMorgan . Niet) props


deMorgan :: Prop -> Prop
deMorgan (Niet (Niet prop)) = deMorgan prop
deMorgan (Niet (En ps))     = Of (negatie ps)
deMorgan (Niet (Of ps))     = En (negatie ps)
deMorgan (a :-> b)          = (deMorgan a) :-> (deMorgan b)
deMorgan prop               = prop

-- "(" should be: '('
-- 109,31-33
