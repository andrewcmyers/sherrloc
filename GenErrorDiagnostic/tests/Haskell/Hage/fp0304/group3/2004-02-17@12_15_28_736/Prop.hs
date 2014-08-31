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
evalueer (Bool b)    _ = b
evalueer (Var a)     bed = if elemBy eqString a bed then True else False
evalueer (En [])     _   = True
evalueer (En (x:xs)) bed = evalueer x bed && evalueer (En xs) bed
evalueer (Of [])     _   = False
evalueer (Of (x:xs)) bed = evalueer x bed || evalueer (Of xs) bed
evalueer (Niet a)    bed = if evalueer a bed then False else True
evalueer (a :-> b)   bed = evalueer (Niet a) bed || evalueer b bed

evalueer' (En p) bed = foldr en True p
                       where en x y = evalueer x bed && evalueer y bed

-- should be: y
-- 29,57-70   29,66-66
