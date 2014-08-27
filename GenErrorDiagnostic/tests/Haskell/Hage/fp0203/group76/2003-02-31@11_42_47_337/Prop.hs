module Prop where

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
evalueer (Bool p) _    = p
evalueer (Var p) xs    | elemBy eqString p xs = True
                       | otherwise   = False
evalueer (Niet p) y    = not (evalueer p y)
evalueer (En p) y      =  and (map (`evalueer` y) p)
evalueer (Of p) y      =  or (map (`evalueer` y) p)
evalueer (p:->q) y     = evalueer (Of [Niet p,q]) y


vervulbaar :: Prop -> [Bedeling]
vervulbaar (Var a)      = [[a]]
vervulbaar (Niet p) = tegenStel p (vervulbaar p)
vervulbaar (En [])      = []
vervulbaar (En (p:ps))  = [concat (vervulbaar p ++ vervulbaar (En ps))]




tegenStel :: Prop -> [Bedeling] -> [Bedeling]
tegenStel p []     = varUitProp p
tegenStel p (x:xs) = verschil eqString (varUitProp p) x : tegenStel p xs

varUitProp :: Prop -> [String]
varUitProp (Var a)     = [a]
varUitProp (Niet p)    = varUitProp p
varUitProp (En [])     = []
varUitProp (En (p:ps)) = verwDubbelString(varUitProp p ++ varUitProp (En ps))
varUitProp (Of [])     = []
varUitProp (Of (p:ps)) = verwDubbelString(varUitProp p ++ varUitProp (Of ps))
varUitProp (p:->q)     = verwDubbelString(varUitProp p ++ varUitProp q)

verwDubbel :: (a->a->Bool) -> [a] -> [a]
verwDubbel _ [] = []
verwDubbel test (x:xs) | elemBy test x xs = verwDubbel test xs
                       | otherwise        = x : verwDubbel test xs

verwDubbelString :: [String] -> [String]
verwDubbelString = verwDubbel eqString


verschil :: (a->a->Bool) -> [a] -> [a] -> [a]
verschil _ x [] = x
verschil _ [] _ = []
verschil test (x:xs) y | elemBy test x y   = verschil test xs y
                       | otherwise  = x : verschil test xs y

-- missing []
-- 38,22-33
