module Propositielogica where

import Data.List

data Prop = En [Prop]
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

evalueer :: Prop -> Bedeling -> Bool

evalueer (Bool p) _      = p
evalueer (Var p) bed     = or (map (eqString p) bed)
evalueer (En p) bed      = and (map (flip evalueer bed) p)
evalueer (Of p) bed      = or (map (flip evalueer bed) p)
evalueer (Niet p) bed    = not (evalueer p bed)
evalueer (p1 :-> p2) bed = not (evalueer p1 bed) || evalueer p2 bed


vervulbaar :: Prop -> [Bedeling]

vervulbaar p =  filter (evalueer p) (subs (getVariables p))


getVariables :: Prop -> [String]

getVariables (Bool _)    = []
getVariables (Var p)     = [p]
getVariables (En p)      = concatMap getVariables p
getVariables (Of p)      = concatMap getVariables p
getVariables (Niet p)    = getVariables p
getVariables (p1 :-> p2) = getVariables p1 ++ getVariables p2


subs :: [a] -> [[a]]

subs []     = [ [] ]
subs (x:xs) = map (x:) subsxs ++ subsxs
        where subsxs = subs xs


tautologie :: Prop -> Bool

tautologie p = and (map (evalueer p) (subs (getVariables p)))


contradictie :: Prop -> Bool

contradictie p = eqList eqString (concat(vervulbaar p)) []


toon :: Prop -> String

toon (Bool True)  = "True"
toon (Bool False) = "False"
toon (Var p)      = p
toon (En p)       = concat (intersperse "/\\" (map toon p))
toon (Of p)       = concat (intersperse "\\/" (map toon p))
toon (Niet p)     = "! " ++ toon p
toon (p1 :-> p2)  = toon p1 ++ " -> " ++ toon p2








deMorgan :: Prop -> Prop

deMorgan (Bool p) = p
deMorgan (Var p) = p
deMorgan (En p) = En p
deMorgan (Of p) = Of p
deMorgan (Niet (Of p)) = (En p)
deMorgan (Niet (En p)) = (Of p)
deMorgan (p1 :-> p2) = p1 :-> p2

-- p should be (En p) and (Of p) respectively
-- 80,21-21   81,20-20
