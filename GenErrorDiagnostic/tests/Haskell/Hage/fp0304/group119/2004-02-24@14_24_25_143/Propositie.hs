module Propositie where

import Data.List

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
showBool       :: Bool -> String
showBool = undefined
data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool
type Bedeling = [String]

subs [] = [ [] ]
subs (x:xs) = map (x:) subsxs ++ subsxs
              where subsxs = subs xs

evalueer :: Prop -> Bedeling -> Bool
evalueer (En xs) b = and $ map (\x -> evalueer x b) xs
evalueer (Of xs) b = or $ map (\x -> evalueer x b) xs
evalueer (Niet x) b = not $ evalueer x b
evalueer (x :-> y) b = not (evalueer x b) || evalueer y b
evalueer (Var x) b = elemBy eqString x b
evalueer (Bool x) _ = x

vervulbaar :: Prop -> [Bedeling]
vervulbaar prop = filter (evalueer prop) (subs (nubBy eqString (variabele prop)))

variabele (En xs) = concatMap variabele xs
variabele (Of xs) = concatMap variabele xs
variabele (Niet x) = variabele x
variabele (x :-> y) = variabele x ++ variabele y
variabele (Var x) = [x]
variabele (Bool x) = []







toon (En xs) = foldl (\x y -> toon x ++ " /\\ " ++ y) [] xs
toon (Of xs) = foldl (\x y -> toon x ++ " \\/ " ++ y) [] xs
toon (Niet x) = "!" ++ toon x
toon (x :-> y) = toon x ++ " -> " ++ toon y
toon (Var x) = x
toon (Bool x) = showBool x

-- toon x should be x; y should be toon y
-- 48,31-36   49,31-36    48,52-52   49,52-52
