module Logica where
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
evalueer (En propositie) bedeling   = and (map (\prop -> evalueer prop bedeling) propositie)
evalueer (Of propositie) bedeling   = or (map (\prop -> evalueer prop bedeling) propositie)
evalueer (Var string) propositie    = elemBy eqString string propositie
evalueer (Bool bool) _              = bool
evalueer (Niet propositie) bedeling = not (evalueer propositie bedeling)
evalueer (prop1 :-> prop2) bedeling = or (not (evalueer prop1 bedeling)):(evalueer prop2 bedeling)

-- the parameter of 'or' should be: [not (evalueer prop1 bedeling),evalueer prop2 bedeling]
-- 22,39-98
