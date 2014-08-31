module Propositielogica where
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




evalueer :: Prop -> Bedeling -> Bool
evalueer (Of x)      y = or (map (flip evalueer y) x)
evalueer (En x)      y = and (map (flip evalueer y) x)
evalueer (Bool x)    _ = x
evalueer (Niet x)    y = not (evalueer x y)
evalueer (xs :-> ys) y = (not (evalueer xs y)) || evalueer ys y
evalueer (Var x)     y = elemBy eqString x y




vervulbaar :: Prop -> [Bedeling]
vervulbaar x = filter (evalueer x) (subs (nubBy eqString (zoekVars x)))

zoekVars :: Prop -> [String]
zoekVars (Of x)      = concat (map zoekVars x)
zoekVars (En x)      = concat (map zoekVars x)
zoekVars (Bool _)    = []
zoekVars (Niet x)    = zoekVars x
zoekVars (xs :-> ys) = zoekVars xs ++ zoekVars ys
zoekVars (Var x)     = [x]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
            where subsxs = subs xs




tautologie :: Prop -> Bool
tautologie x = and (map (evalueer x) (subs (nubBy eqString (zoekVars x))))




contradictie :: Prop -> Bool
contradictie x = not ( or (map (evalueer x) (subs (nubBy eqString (zoekVars x)))))




toon :: Prop -> String
toon (Of x)    = "(" ++ drop 4 (concatMap insertOf (map toon x)) ++ ")"
               where insertOf  = (++) " \\/ "
toon (En x)    = "(" ++ drop 4 (concatMap insertOf (map toon x)) ++ ")"
               where insertOf  = (++) " /\\ "
toon (Niet x)  = "!(" ++ toon x ++ ")"
toon (Bool x)  = x
toon (x :-> y) = "(" ++ toon x ++ " -> " ++ toon y ++ ")"
toon (Var x)   = x

-- x should be "True"
-- 70,18-18
