module Proposia where

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Prop :<-> Prop
  | Var String
  | Bool Bool

type Bedeling = [String]

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined

evalueer               :: Prop -> Bedeling -> Bool
evalueer (Var a) b     =  elemBy eqString a b
evalueer (Bool a) _    =  a
evalueer (En a) b      =  and(map (`evalueer` b) a)
evalueer (Of a) b      =  or (map (`evalueer` b) a)
evalueer (x :-> y) b   =  evalueer (Of [(Niet x), y]) b
evalueer (Niet a) b    =  not(evalueer a b)
evalueer (x :<-> y) b  =  evalueer (Of [(En [(Niet x), (Niet y)]), (En [x , y])]) b









getVars                :: Prop -> [String]
getVars (Var a)        =  [a]
getVars (Bool a)       =  []
getVars (En a)         =  foldr (:) [] (map getVars a)
getVars (Of a)         =  foldr (:) [] (map getVars a)
getVars (x :-> y)      =  (getVars x)++(getVars y)
getVars (Niet a)       =  getVars a
getVars (x :<-> y)     =  (getVars x)++(getVars y)

-- line 39 should have (Of a); line 40 should have (Of a)
-- 39,10-13   40,10-13
