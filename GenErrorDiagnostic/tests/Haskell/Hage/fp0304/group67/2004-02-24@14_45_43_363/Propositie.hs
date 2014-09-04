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

eqString      :: String -> String -> Bool 
eqString = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqChar      :: Char -> Char -> Bool
eqChar = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

evalueer :: Prop -> Bedeling -> Bool
evalueer (En px) bed      = and (map (flip evalueer bed) px)
evalueer (Of px) bed      = or  (map (flip evalueer bed) px)
evalueer (Niet p) bed     = not (evalueer p bed)
evalueer (p1 :-> p2) bed  = evalueer (Of [Niet p1, p2]) bed
evalueer (Var _) []       = False
evalueer (Var s) (b:bx)   | eqString s b = True
                          | otherwise    = evalueer (Var s) bx
evalueer (Bool bool) _    = bool



vervulbaar :: Prop -> [Bedeling]
vervulbaar p = filter (evalueer p) (deellijsten p)

deellijsten :: Prop -> [Bedeling]
deellijsten p = subs (nubBy eqString (variabelen p))

variabelen :: Prop -> Bedeling
variabelen (En px)     = concatMap variabelen px
variabelen (Of px)     = concatMap variabelen px
variabelen (Niet p)    = variabelen p
variabelen (p1 :-> p2) = variabelen p1 ++ variabelen p2
variabelen (Var s)     = [s]
variabelen (Bool _)    = []

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
             where subsxs = subs xs



tautologie :: Prop -> Bool
tautologie p = eqList (eqList eqString) (deellijsten p) (vervulbaar p)

contradictie :: Prop -> Bool
contradictie p = eqList (eqList eqString) [] (vervulbaar p)

toon :: Prop -> String
toon (En px)     = intersperse "/\\" (concatMap toon px)
toon (Of px)     = intersperse "\\/" (concatMap toon px)
toon (Niet p)    = "!" ++ toon p
toon (p1 :-> p2) = toon p1 ++ "->" ++ toon p2
toon (Bool bool) = if eqBool True bool then "T" else "F"

-- should be 'o'
-- 63,32-36   64,32-36
