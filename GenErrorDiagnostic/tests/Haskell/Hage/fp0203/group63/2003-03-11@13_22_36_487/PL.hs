module PL where

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

infixr 1 :->

type Bedeling = [String]

evalueer :: Prop -> Bedeling -> Bool

evalueer (Niet prop) bedeling   = not (evalueer prop bedeling)
evalueer (Bool waarde) _        = waarde
evalueer (Var _) []             = False

evalueer (Var string) (x:xs)  |eqString string x = True
                              |otherwise         = evalueer (Var string) xs
evalueer (p :-> q) bedeling                      = evalueer (Of [Niet p, q]) bedeling

evalueer (Of (p:ps)) bedeling                    = evalueer p bedeling || evalueer (Of ps) bedeling
evalueer (Of []) _ = False

evalueer (En (p:ps)) bedeling                    = evalueer p bedeling && evalueer (En ps) bedeling
evalueer (En []) _ = True

negProp :: Prop -> Prop
negProp (Bool waarde) |waarde    = Bool False
                      |otherwise = Bool True
negProp (Var string) = Niet (Var string)
negProp (pp :-> qq)  = En [pp, negProp qq]
negProp (Niet prop)  = prop
negProp (Of lijst)   = En (map negProp lijst)
negProp (En lijst)   = Of (map negProp lijst)

vervulbaar :: Prop -> [Bedeling]
vervulbaar prop = filter (evalueer prop) ((powerSet.makeUniverse) prop)

powerSet :: Bedeling -> [Bedeling]
powerSet (b:bs)= map ((:) b) (powerSet bs) ++ powerSet bs
powerSet [] = [[]]

removeDupli :: (a -> a -> Bool) -> [a] -> [a]
removeDupli eq (s:ss) |elemBy eq s ss = removeDupli eq ss
                      |otherwise      = s: removeDupli eq ss
removeDupli _ [] = []

makeUniverse :: Prop -> Bedeling
makeUniverse propo = removeDupli eqString (maakUniversum propo)
             where maakUniversum :: Prop -> Bedeling
                   maakUniversum (Bool _) = []
                   maakUniversum (Var string) = [string]
                   maakUniversum (pp :-> qq) = maakUniversum pp ++ maakUniversum qq
                   maakUniversum (Niet prop) = maakUniversum prop
                   maakUniversum (En (p:ps)) = maakUniversum p ++ maakUniversum (En ps)
                   maakUniversum (En [])     = []
                   maakUniversum (Of ps)     = maakUniversum (En ps)

tautologie :: Prop -> Bool
tautologie prop =  and (map (evalueer prop) ((powerSet.makeUniverse) prop))

contradictie :: Prop -> Bool
contradictie prop =  (not.or) (map (evalueer prop) ((powerSet.makeUniverse) prop))

toon :: Prop -> String

toon (Bool waarde) |waarde    = "True"
                   |otherwise = "False"
toon (Var string) = string

toon (Niet prop)  |isOf prop || isImpli prop || isEn prop = "!("++ toon prop ++ ")"
                  |otherwise                              = toon prop

toon (pp :-> qq)  |isImpli pp = "(" ++ toon pp ++ ")" ++ " -> " ++ toon qq
                  |otherwise  = toon pp ++ " -> " ++ toon qq

toon (En [p])     |isOf p || isImpli p = "(" ++ toon p ++ ")"
                  |otherwise           = toon p
toon (En (p:ps))  = toon (En [p]) ++ " /\\ " ++ toon (En ps)

toon (Of [p])     |isImpli p =  "(" ++ toon p ++ ")"
                  |otherwise =  toon p
toon (Of (p:ps))  = toon (Of [p]) ++ " \\/ " ++ toon (Of ps)

isBool :: Prop -> Bool
isBool (Bool _) = True
isBool _        = False

isVar :: Prop -> Bool
isVar (Var _) = True
isVar _       = False

isNiet :: Prop -> Bool
isNiet (Niet _)   = True
isNiet _          = False

isEn :: Prop -> Bool
isEn (En _)  = True
isEn _       = False

isOf :: Prop -> Bool
isOf (Of _)  = True
isOf _       = False

isImpli :: Prop -> Bool
isImpli (_ :-> _)  = True
isImpli _          = False

deMorgan :: Prop -> Prop

deMorgan (En[Var p,Var q]) = Of[(Niet p),(Niet q)]

-- p and q should be: (Var p) and (Var q)
-- 120,39-39   120,48-48
