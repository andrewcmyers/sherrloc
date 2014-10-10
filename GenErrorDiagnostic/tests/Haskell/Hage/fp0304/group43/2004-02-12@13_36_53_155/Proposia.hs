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







vervulbaar             :: Prop -> [Bedeling]
vervulbaar a           =  [ x | x <- y , evalueer a x]
                              where y = subs delDubbels(getVars a)

subs                   :: [a] -> [[a]]
subs []                =  [[]]
subs (x:xs)            =  map (x:) subsxs ++ subsxs
                              where subsxs = subs xs

delDubbels             :: Bedeling -> Bedeling
delDubbels []          =  []
delDubbels (x:xs)      |  elemBy eqString x xs  = delDubbels xs
                       |  otherwise             = x:delDubbels xs

getVars                :: Prop -> Bedeling
getVars (Var a)        =  [a]
getVars (Bool _)       =  []
getVars (En a)         =  concat (map getVars a)
getVars (Of a)         =  concat (map getVars a)
getVars (x :-> y)      =  (getVars x)++(getVars y)
getVars (Niet a)       =  getVars a
getVars (x :<-> y)     =  (getVars x)++(getVars y)

-- missing () around delDubbels(getVars a)
-- 36,46-66   36,41-66   36,41-67
