module HaskellProp where
eqString      :: String -> String -> Bool 
eqString = undefined
data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool
type Bedeling = [String]
evalueer :: Prop -> Bedeling -> Bool
alleVariabelen :: Prop -> Bedeling
verwijderDuplicaten :: Bedeling -> Bedeling
vervulbaar :: Prop -> [Bedeling]
elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
evalueer (En p) bed = all (\x -> evalueer x bed) p
evalueer (Of p) bed = any (\x -> evalueer x bed) p
evalueer (Niet p) bed = not (evalueer p bed)
evalueer (a :-> b) bed = evalueer b bed || (not (evalueer a bed))
evalueer (Var s) bed = elemBy eqString s bed
evalueer (Bool b) _ = b
verwijderDuplicaten [] = []
verwijderDuplicaten (x:xs) | (elemBy eqString x xs) = verwijderDuplicaten xs
                           | otherwise = x: verwijderDuplicaten xs

alleVariabelen (En p) = foldr (++) [] (map alleVariabelen p)
alleVariabelen (Of p) = foldr (++) [] (map alleVariabelen p)
alleVariabelen (Niet p) = alleVariabelen p
alleVariabelen (a :-> b) = (alleVariabelen a) ++ (alleVariabelen b)
alleVariabelen (Var s) = [s]
alleVariabelen (Bool _) = []

alleSubsets :: Bedeling -> [Bedeling]
alleSubsets bed = map ((head bed) :) (alleSubsets (tail bed)) ++ (alleSubsets (tail bed))

vervulbaar (En p) = [alleVariabelen p]
-- p should be (En p)
-- 38,37-37 
