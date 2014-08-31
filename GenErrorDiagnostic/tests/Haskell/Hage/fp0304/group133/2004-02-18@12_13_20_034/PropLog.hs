module PropLog where

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
evalueer (Bool b)  _   = b
evalueer (Var s)   bed = elemBy (eqString) s bed
evalueer (Niet p)  bed = not (evalueer p bed)
evalueer (Of p)    bed = or (map (`evalueer` bed) p)
evalueer (En p)    bed = and (map (`evalueer` bed) p)
evalueer (a :-> b) bed = not (and [(evalueer a bed), not (evalueer b bed)])

vervulbaar :: Prop -> [Bedeling]
vervulbaar _ = [[""]]

geefVar :: Prop -> [String]
geefVar (Var s)   = [s]
geefVar (Bool _)  = [""]
geefVar (Niet p)  = geefVar p
geefVar (a :-> b) = (geefVar a) ++ (geefVar b)
geefVar p         = foldr (++) [""] (map (geefVar) p)

-- p should be: En p or Of p
-- 34,9-9
