module Practium2 (module List, module Practium2) where
import Data.List

eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined
elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool






type Bedeling = [String]


evalueer :: Prop -> Bedeling -> Bool
evalueer (En ps) b                             = foldr (&&) True (map (\x-> evalueer x b) ps )
evalueer (Of ps) b                             = foldr (||) False (map (\x-> evalueer x b) ps )
evalueer (Niet p) b                            = not (evalueer p b)
evalueer (p1 :-> p2) b  | (eqBool (evalueer p1 b) True) && (eqBool (evalueer p2 b) False) = False
                        | otherwise            = True
evalueer (Var ps) b                            = elemBy eqString ps b
evalueer (Bool p) _                            = p







vervulbaar :: Prop -> [Bedeling]
vervulbaar p = filter (evalueer p) (funcBedeling (funcWelkeVariabelen p))


funcBedeling :: [String] -> [Bedeling]
funcBedeling [] = [ [ ] ]
funcBedeling (x:xs) = map (x:) (funcBedeling xs) ++ funcBedeling xs


funcWelkeVariabelen :: Prop -> [String]
funcWelkeVariabelen (En ps)                  = nubBy eqString (concat(map funcWelkeVariabelen ps))
funcWelkeVariabelen (Of ps)                  = nubBy eqString (concat(map funcWelkeVariabelen ps))
funcWelkeVariabelen (Niet p)                 = funcWelkeVariabelen p
funcWelkeVariabelen (p1 :-> p2)              = nubBy eqString (funcWelkeVariabelen p1 ++ funcWelkeVariabelen p2)
funcWelkeVariabelen (Var p)                  = [p]
funcWelkeVariabelen (Bool _)                 = []








tautologie :: Prop -> Bool
tautologie p                                  = and (map (evalueer p) (funcBedeling (funcWelkeVariabelen p)))








contradictie :: Prop -> Bool
contradictie p | eqBool (tautologie (Niet(p))) True = True
               | otherwise                          = False









toon :: Prop -> String
toon (En (p:ps))                             = "(" ++ toon p ++ " /\\ " ++ concatMap toon ps ++ ")"
toon (Of (p:ps))                             = "(" ++ toon p ++ " \\/ " ++ concatMap toon ps ++ ")"
toon (Niet p)                                = "!(" ++ toon p ++ ")"
toon (p1 :-> p2)                             = "(" ++ toon p1 ++ " -> " ++ toon p2 ++ ")"
toon (Var p)                                 = p
toon (Bool p)     | eqBool p True            = "True"
                  | eqBool p False           = "False"









equivalent :: Prop -> Prop -> Bool
equivalent (Var p1) (Var p2)                 = eqString p1 p2
equivalent p1 p2 | length (eqProp p1) == length (eqProp p2) = s p1 p2
                 | otherwise                 = r p1 p2
                 where s q1 q2 | eqList eqBool (eqProp q1) (eqProp q2) = True
                               | otherwise   =  False
                       r (Bool q1) q2        = eqList eqBool (take (length (eqProp q2)) (repeat q1)) (eqProp q2)
                       r q1 (Bool q2)        = eqList eqBool (eqProp q1) (take (length (eqProp q1)) (repeat q2))
                       r (Bool q1) (Bool q2) = eqBool q1 q2
                       r (Niet q1) q2        = eqList eqBool (funcEqList q1 q2) (eqProp q2)
                       r q1 (Niet q2)        = eqList eqBool (eqProp q1) (funcEqList q2 q1)
                       r (Niet q1) (Niet q2) = eqList eqBool (funcEqList q1 q2) (funcEqList q2 q1)
                       funcEqList q1 q2 = concat (map eqProp ( take (length (eqProp q2)) (repeat q1) ))

eqProp :: Prop -> [Bool]
eqProp p = (map (evalueer p) (funcBedeling (funcWelkeVariabelen p)))












deMorgan :: Prop -> Prop
deMorgan (Niet (En ps))                                = Of (map deMorgan ps)
deMorgan (Niet (Of ps))                                = En (map deMorgan ps)
deMorgan (En ps)                                       = Of (map deMorgan ps)
deMorgan (Of ps)                                       = En (map deMorgan ps)
deMorgan (Niet (Niet p))                               = (Niet (p))
deMorgan (Niet p)                                      = p
deMorgan ((Niet p1) :-> (Niet p2))                     = (deMorgan p1 :-> deMorgan p2)
deMorgan ((Niet p1) :-> p2)                            = (deMorgan p1 :-> deMorgan p2)
deMorgan (p1 :-> (Niet p2))                            = (deMorgan p1 :-> deMorgan p2)
deMorgan (p1 :-> p2)                                   = (Of [(Niet (p1)), p2])
deMorgan (Var p)                                       = Niet (Var p)
deMorgan (Bool p)                                      = (Bool p)













ontleed :: String -> Prop
ontleed s = ptoP (higerToProp (opdelenN5(opdelenN4(opdelenN3(volgorde (opdelenN2 (opdelenN1 s)))))))


opdelenN5 :: [[String]] -> [[String]]
opdelenN5 []                                                    = []
opdelenN5 (x:xs) | (length (x:xs) >= 3) && (eqString (head x) "(")  = [head(last(take 2 xs))] : ["("] : [head xs] ++ opdelenN5 [(drop 1 ((last (take 2 (drop 1 xs)))))]
                   | otherwise                                  = x : opdelenN5 xs


opdelenN4 :: [[String]] -> [[String]]
opdelenN4 []                                           = []
opdelenN4 ([""]:xs)                                    = opdelenN4 xs
opdelenN4 (x:xs) | eqString x ""                       = opdelenN4 xs
                 | otherwise                           = x : opdelenN4 xs



opdelenN3 :: [String] -> [[String]]
opdelenN3 []                                           = []
opdelenN3 (a:as) | eqString a "("                      = [a] : opdelenN3 as
                 | eqString a ")"                      = [a] : opdelenN3 as
                 | eqString a "Imp"                    = [a] : opdelenN3 as
                 | otherwise                           = [(take (length(funcSamenVoegen (a:as))-1) (funcSamenVoegen (a:as))) ++ [funcLaatste2 (last (funcSamenVoegen (a:as)))]] ++ [funcLaatste (last (funcSamenVoegen (a:as)))] : opdelenN3 (drop (length(funcSamenVoegen (a:as))) as)
                   where funcSamenVoegen []            = []
                         funcSamenVoegen (x:xs)        | eqString x "("                = [x]
                                                       | eqString x ")"                = [ ]
                                                       | eqString x "Imp"              = [x]
                                                       | otherwise                     = x : funcSamenVoegen xs
                         funcLaatste x                 | eqString x "Imp"              = "Imp"
                                                       | eqString x "("                = "("
                                                       | otherwise                     = ""
                         funcLaatste2 x                | eqString x "("                = []
                                                       | eqString x "Imp"              = []
                                                       | otherwise                     = x


volgorde :: [String] -> [String]
volgorde []                                            = []
volgorde (x:xs) | length xs == 0                       = x : []
                | eqString x                 "("       = x : volgorde xs
                | eqString (last(take 3 xs)) ")"       = x : (take 2 xs) ++ ")" : volgorde (drop 3 xs)
                | eqString (last(take 2 xs)) "En"      = "En" : x : volgorde (take 1 xs ++ drop 2 xs)
                | eqString (last(take 3 xs)) "En"      = "En" : x : volgorde (take 2 xs ++ drop 3 xs)
                | eqString (last(take 2 xs)) "Of"      = "Of" : x : volgorde (take 1 xs ++ drop 2 xs)
                | eqString (last(take 3 xs)) "Of"      = "Of" : x : volgorde (take 2 xs ++ drop 3 xs)
                | otherwise                            = x : volgorde xs


opdelenN2 :: [String] -> [String]
opdelenN2 []                                           = []
opdelenN2 (a:as) | eqString a "Var"                    = funcEnOf (a:as)
                 | otherwise                           = a : opdelenN2 as
                    where funcEnOf (x:xs) | eqString (head xs) "\\/" = "Of" : opdelenN2 (drop 1 xs)
                                          | eqString (head xs) "/\\" = "En" : opdelenN2 (drop 1 xs)
                                          | otherwise                = x    : opdelenN2 xs


opdelenN1 :: String -> [String]
opdelenN1 x | length x == 0 = []
          | eqString (take 1 x) " "                    = opdelenN1 (drop 1 x)
          | eqString (take 1 x) "!"                    = "Niet" : opdelenN1 (drop 1 x)
          | eqString (take 2 x) "->"                   = "Imp"  : opdelenN1 (drop 2 x)
          | eqString (take 1 x) "("                    = "("    : opdelenN1 (drop 1 x)
          | eqString (take 1 x) ")"                    = ")"    : opdelenN1 (drop 1 x)
          | otherwise                                  = "Var"  : (take (lenVar x 1) x) : opdelenN1 (drop (lenVar x 1) x)


lenVar :: String -> Int -> Int
lenVar s i | length s == 0                             = (i-1)
           | eqString (take 1 s) " "                   = (i-1)
           | eqString (take 1 s) "("                   = (i-1)
           | eqString (take 1 s) ")"                   = (i-1)
           | otherwise                                 = lenVar (drop 1 s) (i+1)


ptoP :: [Prop] -> Prop
ptoP []                                                = Bool True
ptoP p                                                 = head (p)


higerToProp :: [[String]] -> [Prop]
higerToProp []                                         = []
higerToProp (x : ["Imp"] : xs )                        = [ (head(toProp x)) :-> (head(toProp (head xs))) ] ++ higerToProp (drop 1 xs)
higerToProp (["Of"]:["("]:x:xs)                        = [Of (toProp x ++ toProp(head xs)) ] ++ higerToProp (drop 1 xs)
higerToProp a@(x:xs) | (length a) == 1                 = toProp (head a)
                     | eqString (head x) ""            = higerToProp xs
                     | eqString (head x) "("           = (toProp (head xs)) ++ higerToProp xs
                     | eqString (head(head xs)) "("    = (toProp (x ++ last(take 2 xs))) ++ higerToProp xs
                     | eqString (head x) "Imp"         = (toProp (head xs)) ++ higerToProp xs
                     | otherwise                       = toProp x ++ higerToProp xs


toProp :: [String] -> [Prop]
toProp []                                              = []
toProp [""]                                            = []
toProp (("En"):strs)                                   = [En (concatMap toProp [strs])]
toProp (("Of"):strs)                                   = [Of (concatMap toProp [strs])]
toProp (("Niet"):strs)                                 = [Niet (head(toProp strs))] ++ toProp (drop 2 strs)
toProp (("Var"):strs)                                  = [Var (head strs)] ++ toProp (drop 1 strs)

-- x should be: (last x) 
-- 174,29-29
