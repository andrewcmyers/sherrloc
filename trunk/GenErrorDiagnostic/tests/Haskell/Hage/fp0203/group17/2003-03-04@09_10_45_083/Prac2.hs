module Prac2 where
eqString      :: String -> String -> Bool 
eqString = undefined
data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

type Bedeling =[String]
evalueer :: Prop->Bedeling->Bool
evalueer(En []) _     = True
evalueer(En(x:xs))bed = and(evalueer x bed: [evalueer(En xs)bed])
evalueer(Of []) _     = False
evalueer(Of(x:xs))bed = or (evalueer x bed: [evalueer(Of xs)bed])
evalueer(Niet x)  bed = not (evalueer x bed)
evalueer(p:->q)   bed = evalueer (Of[Niet p, q]) bed
evalueer(Var s)   bed = not(null(filter (eqString s) bed))
evalueer(Bool c)  _ = c



getVar::Prop->[String]
getVar (Var s)   = [s]
getVar (En [])   = []
getVar (En(x:xs))= getVar x ++ getVar (En xs)
getVar (En [])   = []
getVar (Of(x:xs))= getVar x ++ getVar (Of xs)
getVar (Niet x)  = getVar x





test::[String]
test = getVar (En [Niet(Var "p"), Var "q",True])

-- should be: Bool True
-- 38,43-46
