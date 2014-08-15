module Daveprop where
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
evalueer (Var vartje) [] = False
evalueer (Var vartje) (x:xs)  | eqString vartje x = True
                              | otherwise             = evalueer vartje xs
evalueer (En (x:xs)) bedje = evalueer x bedje && evalueer (En (xs)) bedje
evalueer (Of (x:xs)) bedje = evalueer x bedje || evalueer (Of (xs)) bedje

-- vartje should be: (Var vartje)
-- 17,66-71
