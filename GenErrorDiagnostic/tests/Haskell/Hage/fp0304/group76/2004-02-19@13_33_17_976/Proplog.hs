module Proplog where

data Prop = En [Prop]
          | Of [Prop]
          | Niet Prop
          | Prop :-> Prop
          | Var String
          | Bool Bool

type Bedeling = [String]
type Comb     = (Bool, [String])

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

evalueer :: Prop -> Bedeling -> Bool


evalueer (En (hProp:tProp)) listVar = evalueer hProp listVar && evalueer (En tProp) listVar
evalueer (En []) _ = True


evalueer (Of (hProp:tProp)) listVar = evalueer hProp listVar || evalueer (Of tProp) listVar
evalueer (Of []) _ = False


evalueer (Var s) listVar  | elemBy (eqString) s listVar = True
                          | otherwise = False


evalueer (Bool b) _ | b && True = True
                    | otherwise = False


evalueer (Niet a) listVar = not (evalueer a listVar)


evalueer (a :-> b) listVar = not (evalueer a listVar) || evalueer b listVar





vervulbaar :: Prop -> [Bedeling]

vervulbaar (Var s) = vindTrue (zip (map (evalueer(Var s)) (subs[s])) (subs[s]))

vervulbaar (En a) = vindTrue (zip (map (evalueer(En a)) (subs (extract a))) (subs(extract a)))
vervulbaar (Of a) = vindTrue (zip (map (evalueer(Of a)) (subs (extract a))) (subs(extract a)))






vindTrue :: [Comb] -> [Bedeling]

vindTrue []                = []
vindTrue ((bl,strng):rest) | eqBool True bl = strng : vindTrue rest
                           | otherwise = vindTrue rest


extract :: [Prop] -> [String]

extract (hProp:tProp) = filtreer hProp : extract tProp
extract []            = []


filtreer :: Prop -> [String]

filtreer (Var s) = [s]









subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
         where subsxs = subs xs

-- : should be ++
-- 69,40-40
