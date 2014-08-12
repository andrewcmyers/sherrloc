module Propositie (module List, module Propositie) where
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
notElemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
notElemBy = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined



evalueer :: Prop -> Bedeling -> Bool
evalueer (Bool b) _ = b
evalueer (Var _)  [] = False
evalueer (Var p)  (x:xs) | eqString p x = True
                         | otherwise    = evalueer (Var p) xs
evalueer (Niet p) xs  = not(evalueer p xs)
evalueer (En ps)  xs  = and(map f ps)
           where f p  = evalueer p xs
evalueer (Of ps) xs   = or(map f ps)
           where f p  = evalueer p xs
evalueer (p :-> q) xs = evalueer (Of [Niet (p), q]) xs


proposities :: Prop -> [String]
proposities (Bool _)  = []
proposities (Var p)   = [p]
proposities (Niet p)  = proposities p
proposities (En ps)   = concatMap proposities ps
proposities (Of ps)   = concatMap proposities ps
proposities (p :-> q) = proposities (Of [p, q])


varLijst :: [String] -> [String]
varLijst [] = []
varLijst (x:xs) | notElemBy (eqString) x xs = [x] ++ varLijst xs
                | otherwise                 = varLijst xs



subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
            where subsxs = subs xs


vervulbaar :: Prop -> [Bedeling]
vervulbaar p   = filter (evalueer p) (subs (varLijst (proposities p)))



tautologie :: Prop -> Bool
tautologie p  = length (vervulbaar (Niet p)) == 0



contradictie :: Prop -> Bool
contradictie p = length (vervulbaar p) == 0


toon :: Prop -> String
toon p = tonen p 4

tonen :: Prop -> Int -> String
tonen (Bool b) _ | b         = "T"
                     | otherwise = "F"
tonen (Var p) _ = p
tonen (Niet p) level | level < 1 = "!(" ++ tonen p 1 ++ ")"
                     | otherwise = "!" ++ tonen p 1
tonen (En ps) level | length ps == 0 = ""
                    | length ps == 1 = tonen (head ps) 2
                    | level < 2      = "(" ++ tonen (head ps) 2 ++ "/\\" ++ tonen (En (tail ps)) 2 ++ ")"
                    | otherwise      = tonen (head ps) 2 ++ "/\\" ++ tonen (En (tail ps)) 2
tonen (Of ps) level | length ps == 0 = ""
                    | length ps == 1 = tonen (head ps) 3
                    | level < 3      = "(" ++ tonen (head ps) 3 ++ "\\/" ++ tonen (Of (tail ps)) 3 ++ ")"
                    | otherwise      = tonen (head ps) 3 ++ "\\/" ++ tonen (Of (tail ps)) 3
tonen (p :-> q) _ = tonen p 4 ++ "->" ++ tonen q 4


equivalent :: Prop -> Prop -> Bool
equivalent p q = null (filter (geenEquivalentie p q) (subs (varLijst (proposities p ++ proposities q))))

geenEquivalentie :: Prop -> Prop -> Bedeling -> Bool
geenEquivalentie p q b = not (eqBool (evalueer p b) (evalueer q b))


deMorgan :: Prop -> Prop
deMorgan (Bool b) = Bool b
deMorgan (Var p)  = Var p
deMorgan (Niet (Var p)) = Niet Var p
deMorgan (En ps) = En (map deMorgan ps)
deMorgan (Of ps) = Of (map deMorgan ps)
deMorgan (Niet (En [p, q])) = Of [deMorgan (Niet p), deMorgan (Niet q)]
deMorgan (Niet (Of [p, q])) = En [deMorgan (Niet p), deMorgan (Niet q)]

-- missing ()
-- 102,32-36
