module Propositielogica where

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

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

evalueer :: Prop -> Bedeling -> Bool
evalueer (Bool x) _ = x
evalueer (Var p) lijst = elemBy eqString p lijst
evalueer (Niet p) bedeling = not(evalueer p bedeling)
evalueer (Of [] ) _ = False
evalueer (Of (x:xs)) bedeling = evalueer x bedeling || evalueer (Of xs) bedeling
evalueer (En [] ) _ = True
evalueer (En (x:xs)) bedeling = evalueer x bedeling && evalueer (En xs) bedeling
evalueer (p :-> q) bedeling        |evalueer p bedeling && evalueer q bedeling = True
                                   |evalueer (Niet p) bedeling = True
                                   |evalueer p bedeling && evalueer (Niet q) bedeling = False
                                   |otherwise = True


subs :: Bedeling -> [Bedeling]
subs [] = [[]]
subs  (x:xs) = map (x:) (subs xs) ++ subs xs


maakAlles :: Prop -> [String]
maakAlles (Bool _)  = []
maakAlles (Var p)   = [p]
maakAlles (En (x:xs)) = maakAlles x ++ maakAlles (En xs)
maakAlles (En [])   = []
maakAlles (Of prop)   = maakAlles (En prop)
maakAlles (Niet prop) = maakAlles prop
maakAlles (p :-> q)   = maakAlles p ++ maakAlles q


removeDupli :: [String] -> [String]
removeDupli [] = []
removeDupli (s:ss) |elemBy eqString s ss = removeDupli ss
                   |otherwise      = s: removeDupli ss


maakAllesEnSchoon :: Prop -> Bedeling
maakAllesEnSchoon prop = removeDupli (maakAlles prop)



vervulbaar :: Prop -> [Bedeling]
vervulbaar prop = filter (evalueer prop) ((subs.maakAllesEnSchoon) prop)


tautologie :: Prop -> Bool
tautologie prop    |vergelijkLijstBedeling (vervulbaar prop)(subs(maakAllesEnSchoon prop)) = True
                   |otherwise = False



vergelijkLijstBedeling :: [Bedeling] -> [Bedeling] -> Bool
vergelijkLijstBedeling (x:xs) volleProp            |length(filter(eqBool True)(map (eqList x) volleProp)) == 1 = True
                                                   |otherwise = False

-- should be (eqList eqString x)
-- 75,85-90   75,92-92   75,85-92
