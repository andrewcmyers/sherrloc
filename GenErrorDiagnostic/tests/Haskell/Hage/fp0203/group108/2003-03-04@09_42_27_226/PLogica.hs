module PLogica where
eqString      :: String -> String -> Bool 
eqString = undefined
data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool
varErAf :: Prop -> String
varErAf (Var x) = x

vergelijk :: String -> String -> Bool
vergelijk x y = varErAf x `eqString` y

-- type should be Prop -> String -> String
-- 14,1-9   14,14-37   14,14-19   14,34-37
