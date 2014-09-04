module Practicum2 where

import Data.List hiding (delete)

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

data Prop
 = En [Prop]
 | Of [Prop]
 | Niet Prop
 | Prop :-> Prop
 | Var String
 | Bool Bool

type Bedeling = [String]


evalueer :: Prop -> Bedeling -> Bool
evalueer = flip eval

eval :: Bedeling -> Prop -> Bool
eval bedeling (Var  x   ) = elemBy eqString x bedeling
eval _        (Bool x   ) = x
eval bedeling (Niet x   ) = not (eval bedeling x)
eval bedeling (En   x   ) = and (map (eval bedeling) x)
eval bedeling (Of   x   ) = or (map (eval bedeling) x)
eval bedeling ((x :-> y)) = eval bedeling (Niet x) || eval bedeling y

containsVars :: Prop -> Bedeling
containsVars prop = delDoubles (eqList eqString) (containsVars1 prop)

containsVars1 :: Prop -> Bedeling
containsVars1 (Var x)         = [x]
containsVars1 (En (x: xs)) = containsVars1 x ++ containsVars1 (En xs)
containsVars1 (Of (x: xs)) = containsVars1 x ++ containsVars1 (Of xs)
containsVars1 (Niet prop)     = containsVars1 prop
containsVars1 (a :-> b)       = containsVars1 a ++ containsVars1 b
containsVars1 (Bool _)        = []

containsVars1 _ = []


alleMogelijkheden :: Prop -> [Bedeling]
alleMogelijkheden prop = subs (containsVars prop)

oplossingen :: Prop -> [Bedeling]
oplossingen prop = [x| x<- alleMogelijkheden prop, evalueer prop x]

contradictie :: Prop -> Bool
contradictie prop = null [x | x <- oplossingen prop, evalueer prop x]

tautologie :: Prop -> Bool
tautologie prop = eqList (eqList eqString) (oplossingen prop) (alleMogelijkheden prop)


eqProp :: Prop -> Prop -> Bool

eqProp (En (p:rop1)) (En (pr:op2)) = eqProp p pr && eqProp (En rop1) (En op2)
eqProp (En [])       (En [])       = True

eqProp (Of (p:rop1)) (Of (pr:op2)) = eqProp p pr && eqProp (Of rop1) (Of op2)
eqProp (Of [])       (Of [])       = True

eqProp (Niet x)  (Niet y)  = eqProp x y
eqProp (a :-> b) (c :-> d) = eqProp a c && eqProp b d
eqProp (Var x)   (Var y)   = eqString x y
eqProp (Bool x)  (Bool y)  = eqBool x y

eqProp _ _ = False


subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
        where subsxs = subs xs


delete :: (a -> Bool) -> [a] -> [a]
delete f = filter (not.f)


delDoubles :: (a -> a -> Bool) -> [a] -> [a]
delDoubles _ [] = []
delDoubles f (l:ist) = l:delDoubles f (delete (f l) ist)

-- (eqList eqString) should be: eqString
-- 39,33-47   39,40-47
