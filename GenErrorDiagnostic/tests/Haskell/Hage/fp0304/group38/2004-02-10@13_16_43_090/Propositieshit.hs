module Propositieshit where

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


evalueer :: Prop -> Bedeling -> Bool
evalueer _ = True

varInLijst :: String -> Bedeling -> Bool
varInLijst a []      = False
varInLijst a (x:xs)  | eqString a x = True
                     | otherwise    = False

-- should match: _ _
-- 18,10-10
