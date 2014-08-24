module PropLogica (module PropLogica, module List) where
import Data.List

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
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined



evalueer :: Prop -> Bedeling -> Bool
evalueer (Bool bool) bed = bool
evalueer (Var str) bed = elemBy eqString str bed
evalueer (Niet prop) bed | (evalueer prop bed) `eqBool` True = False
                         | otherwise = True
evalueer (En props) bed = and(map (flip evalueer props) bed)

-- and(map (flip evalueer props) bed) should be: and(map ((flip evalueer) bed) props)
-- 28,50-54   28,57-59
