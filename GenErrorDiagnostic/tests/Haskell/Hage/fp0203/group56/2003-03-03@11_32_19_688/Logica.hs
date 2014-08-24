module Logica where
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

evalueer (Bool b) _ = b

evalueer (Var _) [] = False
evalueer (Var y) (x:xs) | eqString x y = True
                        | otherwise    = evalueer (Var y) xs

evalueer (Niet z) bed = not (evalueer z bed)

evalueer (En []) _ = True
evalueer (En (x:xs)) bed = evalueer x bed && evalueer (En xs) bed

evalueer (Of []) _ = False
evalueer (Of (x:xs)) bed = evalueer x bed || evalueer (Of xs) bed

evalueer (a :-> b) bed = evalueer (Niet a) bed || evalueer b bed





vervulbaar :: Prop -> [Bedeling]
vervulbaar (Bool _) = []
vervulbaar (Var x) = [[x]]
vervulbaar (x :-> y) = concat (vervulbaar x ++ vervulbaar y) : vervulbaar y ++ [[]]
vervulbaar (Niet _) =  []
vervulbaar (En []) = []
vervulbaar (En (x:xs)) =  concat((vervulbaar x) ++ (vervulbaar (En (xs)))) : []
vervulbaar (Of []) = []
vervulbaar (Of (x:xs)) =  (concat(concat((vervulbaar x) ++ (vervulbaar (En (xs))))) : [])

-- remove "concat"
-- 46,35-82
