module Logica (module Logica, module List) where
import Data.List

eqString      :: String -> String -> Bool 
eqString = undefined
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
evalueer (Bool bool)              _   = bool
evalueer (En [])                  _   = True
evalueer (Of [])                  _   = True
evalueer (Var string)             bed = hulp string bed
evalueer (Niet prop)              bed = not (evalueer prop bed)
evalueer (En (prop1 : proprest))  bed = evalueer prop1 bed && evalueer (head (proprest)) bed
evalueer (Of (prop1 : proprest))  bed = evalueer prop1 bed || evalueer (head (proprest)) bed
evalueer (prop1 :-> prop2)        bed = evalueer (Niet prop1) bed || evalueer prop2 bed

hulp :: String -> Bedeling -> Bool
hulp _ [] = False
hulp string (bed:beds) | (eqString string bed) = True
                       | otherwise             = hulp string beds




geefAlleVars :: Prop -> Bedeling
geefAlleVars (Var s)   = [s]
geefAlleVars (Bool _)  = []
geefAlleVars (Niet s)  = geefAlleVars s
geefAlleVars (En s)    = concatMap geefAlleVars s
geefAlleVars (Of s)    = geefAlleVars (En s)
geefAlleVars (s :-> t) = geefAlleVars (En [s,t])




combineer :: Bedeling -> [Bedeling]
combineer [] = [[]]
combineer (bed : beds) = let prev = combineer beds
                         in (map ((:) bed) prev) ++ prev

maakTuples :: [Bedeling]-> [Bool] -> [(Bedeling, Bool)]
maakTuples [] _                    = []
maakTuples (bed:beds) (bool:bools) = zip (bed:beds) (bool:bools)



testBool :: [Bedeling] -> Prop -> [Bool]
testBool [] _            = [False]
testBool (bed:beds) prop = evalueer prop bed : testBool beds prop

pakTrues:: [(Bedeling,Bool)] -> [(Bedeling,Bool)]
pakTrues []                = []
pakTrues ((bed,bool):rest) = if eqBool bool True then (bed,bool): pakTrues rest
                             else pakTrues rest

pakEerste :: [(Bedeling,Bool)] -> [Bedeling]
pakEerste [] = []
pakEerste ((bed,_):xs) = bed : pakEerste xs



vervulbaar :: Prop -> [Bedeling]
vervulbaar prop = pakEerste (pakTrues (maakTuples (combineer (geefAlleVars prop)) (testBool (combineer (geefAlleVars prop)) (prop))))





tautologie :: Prop -> Bool
tautologie prop = length (vervulbaar prop) == length (combineer (geefAlleVars prop))

contradictie :: Prop -> Bool
contradictie prop = length (vervulbaar prop) == 0

equivalent :: Prop -> Prop -> Bool
equivalent p q = tautologie (En [(p :-> q) , (q :-> p)])




toon :: Prop -> String
toon (Of [En props1, En props2]) = "(" ++ (concat (intersperse " /\\ " (map toon props1))  ++ " \\/ " ++
                                           concat (intersperse " /\\ " (map toon props2))) ++ ")"
toon (Of [prop, Of props])       = "(" ++ toon prop ++ " \\/ " ++ concat (intersperse " \\/ " (map toon props)) ++ ")"
toon (En [prop, En props])       = "(" ++ toon prop ++ " /\\ " ++ concat (intersperse " /\\ " (map toon props)) ++ ")"
toon (Of [En props, prop])       = "(" ++ concat (intersperse " /\\ " (map toon props)) ++ " \\/ " ++ prop      ++ ")"




toon (En [])        = ""
toon (Var p)        = p
toon (Bool True)    = "True"
toon (Bool False)   = "False"
toon (Niet p)       = "!"  ++ toon p
toon (p :-> q)      = "("  ++ toon p ++ " -> " ++ toon q ++ ")"
toon (En props)     = "("  ++ concat (intersperse " /\\ " (map toon props)) ++ ")"
toon (Of props)     = "("  ++ concat (intersperse " \\/ " (map toon props)) ++ ")"

deMorgan :: Prop -> Prop
deMorgan (Var string)                = Var string
deMorgan (Bool bool)                 = Bool bool
deMorgan (Niet (Var string))         = Niet (Var string)
deMorgan (Niet (Bool False))         = Bool True
deMorgan (Niet (Bool True))          = Bool False
deMorgan (Niet (Niet prop))          = prop
deMorgan (En props)                  = En (map deMorgan props)
deMorgan (Of props)                  = Of (map deMorgan props)
deMorgan (Niet (En props))           = deMorgan (Of (map Niet props))
deMorgan (Niet (Of props))           = deMorgan (En (map Niet props))
deMorgan (prop1 :-> prop2)           = deMorgan (Of [Niet prop1 , prop2])

-- prop should be: toon prop
-- 98,103-106
