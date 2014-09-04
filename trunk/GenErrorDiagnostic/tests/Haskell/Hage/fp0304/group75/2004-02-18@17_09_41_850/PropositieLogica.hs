module PropositieLogica where

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















isVar :: Prop -> Bool
isVar (Var a) = True
isVar (Bool a) = False
isVar (En a) = False
isVar (Of a) = False
isVar (Niet a) = False

isBool :: Prop -> Bool
isBool (Var a) = False
isBool (Bool a) = True
isBool (En a) = False
isBool (Of a) = False
isBool (Niet a) = False

evalueer :: Prop -> [String] -> Bool
evalueer (Bool b) _     = b
evalueer (Var a) []     = False
evalueer (Var a) x |elemBy eqString a x = True
                   |otherwise = False
evalueer (En[]) _      = True
evalueer (En (x:xs)) y |isVar x   = evalueer (Var x) y  && evalueer (En xs) y
                       |isBool x  = evalueer (Bool x) && evalueer (En xs) y
                       |otherwise = evalueer x y

-- (Var x) should be x
-- 52,47-49   52,51-51
