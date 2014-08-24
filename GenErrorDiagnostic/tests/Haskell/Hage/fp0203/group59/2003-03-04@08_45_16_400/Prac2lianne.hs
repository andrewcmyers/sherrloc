module Prac2lianne where
data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined















type Bedeling = [String]
lijst :: Bedeling
lijst = ["p","q"]


evalueer :: Prop -> Bedeling -> Bool

evalueer (Var string) bedeling = elemBy eqString string bedeling
evalueer (En [a,b]) bedeling   = evalueer a bedeling && evalueer b bedeling
evalueer (Of [a,b]) bedeling   = evalueer a bedeling || evalueer b bedeling
evalueer (Niet a) bedeling     = not (evalueer a bedeling)
evalueer (a :-> b) bedeling      | (evalueer a bedeling && evalueer b bedeling) = True
                                 | (evalueer a bedeling `eqBool` False )        = True
                                 | otherwise                                    = False
evalueer (Bool a) _            = a
















alleVariabelen :: Prop -> Bedeling
alleVariabelen (Var string) = [string]
alleVariabelen (En [a,b]) = concat (alleVariabelen a ++ alleVariabelen b)

-- should delete 'concat'
-- 64,29-73
