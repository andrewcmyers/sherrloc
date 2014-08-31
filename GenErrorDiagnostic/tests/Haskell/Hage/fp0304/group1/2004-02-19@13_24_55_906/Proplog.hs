module Proplog where

data Prop = En [Prop]
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
type Comb     = (Bool, [String])


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
vervulbaar (En (h:t)) = vervulbaar Var(h) ++ vervulbaar (En(t))




vindTrue :: [Comb] -> [Bedeling]

vindTrue []                = []
vindTrue ((bl,strng):rest) | eqBool True bl = strng : vindTrue rest
                           | otherwise = vindTrue rest




subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
         where subsxs = subs xs

-- Var(h) should be: (h)
-- 52,36-38   52,36-41
