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
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

evalueer :: Prop -> Bedeling -> Bool

evalueer (Var prop) (x : xs) | eqString prop x = True
                             | otherwise = evalueer (Var prop) xs
evalueer (Var _) [] = False

evalueer (Niet prop) lijst = not (evalueer prop lijst)

evalueer (Bool boolean) _ | eqBool boolean True = True
                          | otherwise = False

evalueer (En []) _ = True
evalueer (En (kopprop:restprop)) lijst = (evalueer kopprop lijst) && (evalueer (En restprop) lijst)

evalueer (Of []) _ = False
evalueer (Of (kopprop:restprop)) lijst = (evalueer kopprop lijst) || (evalueer (En restprop) lijst)

evalueer (prop1 :-> prop2) lijst = evalueer (Of ((Niet prop1) : [prop2])) lijst


















geefVarTerug :: Prop -> [String ]
geefVarTerug (Var prop) = [prop]
geefVarTerug (Niet prop) = geefVarTerug prop
geefVarTerug (En []) = []
geefVarTerug (En (kopprop:restprop)) = (geefVarTerug kopprop) ++ (geefVarTerug (En restprop))
geefVarTerug (Of []) = []
geefVarTerug (Of (kopprop:restprop)) = (geefVarTerug kopprop) ++ (geefVarTerug (Of restprop))
geefVarTerug (prop1 :-> prop2) = geefVarTerug (Of ((Niet prop1) : [prop2]))





tautologie :: Prop -> Bool

tautologie (Var prop) = evalueer (Var prop)

-- should return False
-- 66,25-43
