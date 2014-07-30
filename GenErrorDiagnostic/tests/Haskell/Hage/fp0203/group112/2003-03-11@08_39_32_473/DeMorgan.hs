module DeMorgan (module DeMorgan, module List) where

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
 :: Prop -> Bedeling -> Bool
evalueer (Bool b) _ = b
evalueer (Var s) bed | elemBy eqString s bed = True
                     | otherwise = False
evalueer (En x) bed = and (map (\y -> evalueer y bed) x)
evalueer (Of x) bed = or (map (\y -> evalueer y  bed) x)
evalueer (Niet x) bed = not(evalueer x bed)
evalueer (a :-> b) bed | evalueer a bed && evalueer b bed = True
                       | evalueer a bed && not (evalueer b bed) = False
                       | otherwise = True




vervulbaar :: Prop -> [Bedeling]
vervulbaar x = filter (evalueer x) (alleBedeling x)



deelRij :: [a] -> [[a]]
deelRij [] = [[]]
deelRij (x:xs) = (deelRij xs) ++ map(x:)(deelRij xs)

propToString :: Prop -> [String]
propToString (Bool _) = []
propToString (Var s) = [s]
propToString (a :-> b) = nubBy eqString (propToString a ++ propToString b)
propToString (En x) = nubBy eqString (concatMap propToString x)
propToString (Of x) = nubBy eqString (concatMap propToString x)
propToString (Niet x) = nubBy eqString (propToString x)

alleBedeling :: Prop -> [[String]]
alleBedeling x = deelRij (propToString x)




tautologie :: Prop -> Bool
tautologie x = null (vervulbaar (Niet x))




contradictie :: Prop -> Bool
contradictie x = null (vervulbaar x)



equivalent :: Prop -> Prop -> Bool
equivalent x y = and (map (evalueer x) (vervulbaar y)) && and (map (evalueer y) (vervulbaar x))














deMorgan :: Prop -> Prop
deMorgan (Var s) = (Var s)
deMorgan (Bool b) = (Bool b)
deMorgan (Niet x) = (check x)

deMorgan (En x) = (En (map (deMorgan x)))
deMorgan (Of x) = (Of (map (deMorgan x)))






negeerAlle :: [Prop] -> [Prop]
negeerAlle [] = []
negeerAlle x = (Niet (head x):negeerAlle (tail x))

check :: Prop -> Prop
check (Of x) = (En (map deMorgan (negeerAlle x)))
check (En x) = (Of (map deMorgan (negeerAlle x)))
check (Var s) = (Niet (Var s))
check (Bool b) =  (Bool (not b))
check (a :-> b) = Niet (a :-> b)
check (Niet x) = Niet( Niet x)

-- (deMorgan x) --> deMorgan x
-- 86,28-39    87,28-39
