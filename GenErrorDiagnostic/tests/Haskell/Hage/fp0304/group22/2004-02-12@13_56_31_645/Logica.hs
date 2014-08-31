module Logica where

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

evalueer :: Prop -> Bedeling -> Bool
evalueer (Bool b)  _   = b
evalueer (Var  v)  bed = elemBy eqString v bed
evalueer (En ps)   bed = and (map (`evalueer` bed) ps)
evalueer (Of ps)   bed = or (map (`evalueer` bed) ps)
evalueer (Niet p)  bed = not (evalueer p bed)
evalueer (p :-> q) bed = evalueer (Of [Niet p,q]) bed

vervulbaar :: Prop -> [Bedeling]
vervulbaar p = filter (evalueer p) (alleBedelingen p)


alleBedelingen :: Prop -> [Bedeling]
alleBedelingen p = subs (elimDuplicates (variabelen p))

variabelen :: Prop -> [String]
variabelen (Bool _)  = []
variabelen (Var v)   = [v]
variabelen (En ps)   = concatMap variabelen ps
variabelen (Of ps)   = concatMap variabelen ps
variabelen (Niet p)  = variabelen p
variabelen (p :-> q) = variabelen p ++ variabelen q






subs :: [a] -> [[a]]
subs [] = [ [] ]
subs (x:xs) = map (x:) subsxs ++ subsxs
            where subsxs = subs xs


elimDuplicates :: (a -> a -> Bool) -> [a] -> [a]
elimDuplicates eq = foldr (elimD) []
                  where elimD x ys | elemBy eq x ys = ys
                                   | otherwise      = x : ys



test1 :: Prop


test1 = (Of [En [Var "p", Var "q"],Var "joop"])


-- should be eqString (..)
-- 31,42-53
