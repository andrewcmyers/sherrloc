module Propositielogica where
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


evalueer :: Prop -> Bedeling -> Bool
evalueer (_:->_) []         = True
evalueer _ []               = False
evalueer (Var x) (bed:beds) = (eqString x bed) || (evalueer (Var x) beds)
evalueer (Bool b) _         = b


evalueer (En ys) bed = foldr (\y->((evalueer y bed) &&)) True ys

evalueer (Of []) _        = False
evalueer (Of (x:lijst)) bed = (evalueer x bed) || (evalueer (Of lijst) bed)



evalueer (x:->y) bed    = (evalueer (Niet x) bed) || (evalueer y bed)
evalueer (Niet x) bed   = (not.evalueer x) bed


getVars :: Prop -> [String]
getVars (Bool _)   = []
getVars (Var s)    = [s]
getVars (Niet p)   = getVars p
getVars (En lijst) = concatMap getVars lijst
getVars (Of lijst) = concatMap getVars lijst
getVars (x :-> y)  = (getVars x) ++ (getVars y)


verwijderDubbele :: [String] -> [String]
verwijderDubbele [] = []
verwijderDubbele (x:xs) | elemBy eqString x xs = verwijderDubbele xs
                        | otherwise = x:(verwijderDubbele xs)


subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
                   where subsxs = subs xs



vervulbaar :: Prop -> [Bedeling]
vervulbaar p = filter (evalueer p) (subs(verwijderDubbele(getVars p)))


tautologie :: Prop -> Bool
tautologie p = contradictie (Niet p)


contradictie :: Prop -> Bool
contradictie = null.vervulbaar


getPrio :: Prop -> Int

getPrio (Niet _) =  9
getPrio (En _)   =  8
getPrio (Of _)   =  7
getPrio (_ :-> _)=  6
getPrio (_)      =  1











toon :: Prop -> String


toon (En [p]) = toon p
toon (En (p:ps))
                 | (getPrio (En[])) < p = enclose s
                 | otherwise = s
                 where s = (toon p) ++ " /\\ " ++ (toon (En ps))
toon (Of [p]) = toon p
toon (Of (p:ps))
                 | (getPrio (Of[])) < getPrio p = enclose s
                 | otherwise = s
                 where s = (toon p) ++ " \\/ " ++ (toon (Of ps))
toon (Niet p)    | (getPrio(Niet p)) < (getPrio p) = enclose s
                 | otherwise = s
                 where s = "!" ++ toon p
toon (Bool _)     = "Bool"
toon (Var p)      = "" ++ p ++ ""
toon (x :-> y)    |(getPrio(x:->y)) < maximum[(getPrio x),(getPrio y)] = enclose s
                  | otherwise = s
                  where s = (toon x) ++ " -> " ++ (toon y)












enclose :: String -> String
enclose s = "(" ++ s ++ ")"


equivalent :: Prop -> Prop -> Bool

equivalent p q =  tautologie(En[(p :-> q), (q :-> p)])

deMorgan                  :: Prop -> Prop
deMorgan (Var v)           = Var v
deMorgan (Bool b)          = Bool b
deMorgan (a :-> b)         = (deMorgan a :-> deMorgan b)
deMorgan (Of lijst)        = Of (map deMorgan lijst)
deMorgan (En lijst)        = En (map deMorgan lijst)
deMorgan (Niet(En lijst))  = Of (map f lijst)
         where f p         = Niet (deMorgan p)
deMorgan (Niet(Of lijst))  = En (map f lijst)
         where f p         = Niet (deMorgan p)
deMorgan (Niet p)          = Niet (deMorgan p)

-- p should be: getPrio p
-- 93,39-39
