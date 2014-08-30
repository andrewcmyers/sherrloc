module Logic where
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

type Bedeling = [String]
evalueer :: Prop -> Bedeling -> Bool
evalueer (En []) _ = True
evalueer (En (x:xs)) bed = case x of
                           Bool y -> y && evalueer (En xs) bed
                           Var  y -> (elemBy eqString y  bed) && evalueer (En xs) bed
                           En   y -> evalueer (En y) bed && evalueer (En xs) bed
                           Of   y -> evalueer (Of y) bed && evalueer (En xs) bed
                           Niet y -> (not.evalueer y bed ) && evalueer (En xs) bed

-- not.evalueer y bed should be: not (evalueer y bed )
-- 23,39-56
