module Logica where
import Data.List

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

type Bedeling = [String]







waar :: Bedeling -> String -> Bool
waar b v = elemBy eqString v b


evalueer :: Prop -> Bedeling -> Bool
evalueer (En ps) b     = and $ map (flip evalueer $ b) ps
evalueer (Of ps) b     = or  $ map (flip evalueer $ b) ps
evalueer (Niet p) b    = not $ evalueer p b
evalueer (p1 :-> p2) b = (not $ evalueer p1 b) || evalueer p2 b
evalueer (Var v) b     = waar b v
evalueer (Bool bool) _ = bool






vars :: Prop -> [String]
vars (En ps)     = concat $ map vars ps
vars (Of ps)     = concat $ map vars ps
vars (Niet p)    = vars p
vars (p1 :-> p2) = vars p1 ++ vars p2
vars (Var v)     = [v]
vars (Bool _)    = []


uniqueVars :: Prop -> [String]
uniqueVars = nubBy eqString . vars


bedelingen :: [String] -> [Bedeling]
bedelingen [] = [[]]
bedelingen (v:vs) = map (v:) (bedelingen vs) ++ bedelingen vs


vervulbaar :: Prop -> [Bedeling]
vervulbaar p = [ b | b <- bedelingen $ uniqueVars p, evalueer p b ]






tautologie :: Prop -> Bool
tautologie p = (length $ vervulbaar p) == (length $ bedelingen $ uniqueVars p)






contradictie :: Prop -> Bool
contradictie = null . vervulbaar





boolToString :: Bool -> String
boolToString True = "T"
boolToString False = "F"

toon :: Prop -> String
toon (En ps) = "(" ++ (intersperse " /\\ " $ map toon ps) ++ ")"
toon (Of ps) = "(" ++ (intersperse " \\/ " $ map toon ps) ++ ")"
toon (Niet p) = "(!" ++ toon p ++ ")"
toon (p1 :-> p2) = "(" ++ toon p1 ++ " -> " ++ toon p2 ++ ")"
toon (Var v) = "(" ++ v ++ ")"
toon (Bool b) = "(" ++ boolToString b ++ ")"

-- (intersperse " /\\ " $ map toon ps) should be:  (concat $ intersperse " /\\ " $ map toon ps)
-- 88,24-56   89,24-56    88,59-60    89,59-60
