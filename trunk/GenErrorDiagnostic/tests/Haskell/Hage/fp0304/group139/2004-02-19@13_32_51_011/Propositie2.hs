module Propositie2 where

data Prop = En [Prop] | Of [Prop] | Niet Prop | Prop :-> Prop | Var String | Bool Bool

type Bedeling = [String]

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined

evalueer :: Prop -> Bedeling -> Bool
evalueer (En xs) b = and $ map (`evalueer` b) xs
evalueer (Of xs) b = or  $ map (`evalueer` b) xs
evalueer (Niet a) ys = (not (evalueer a ys))
evalueer (a :-> b) ys = ( evalueer a ys || (not (evalueer b ys)))
evalueer (Var a) (y:ys) | a `eqString` y = True
                        | otherwise = evalueer (Var a) ys
evalueer (Bool a) _ = a
evalueer _ [] = False


vervulbaar :: Prop -> [Bedeling]
vervulbaar p = [ x |x <- subs (variabelen p), evalueer p x] ++[[]]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
              where subsxs = subs xs

variabelen :: Prop -> Bedeling
variabelen (En xs) = verwijderDubbele (concatMap variabelen xs)
variabelen (Of xs) = verwijderDubbele (concatMap variabelen xs)
variabelen (Niet xs) = variabelen xs
variabelen (a :-> b) = verwijderDubbele (variabelen a ++ variabelen b)
variabelen (Var a) = [a]
variabelen (Bool _) = []

verwijderDubbele :: [String] -> [String]
verwijderDubbele [] = []
verwijderDubbele (x:xs) | elemBy eqString x xs = verwijderDubbele xs
                        | otherwise        = x : verwijderDubbele xs




















tautologie ::Prop -> Bool
tautologie p = [ x |x <- subs (variabelen p), not(evalueer p x)] `eqStrLists` []

contradictie :: Prop -> Bool
contradictie a = (vervulbaar a) `eqStrLists` [[]]














zetTussen :: [String] -> String -> String
zetTussen xs str= (concatMap (++ str) (init xs)) ++ tail xs





equivalentie :: Prop -> Prop -> Bool
equivalentie a b | (vervulbaar a) `eqStrLists` (vervulbaar b) = True
                 | otherwise = False


eqStrLists :: [[String]] -> [[String]] -> Bool
eqStrLists [[]] [[]] = True
eqStrLists _ [[]] = False
eqStrLists [[]] _ = False
eqStrLists lvl1 lvl2 = eqList eqString (concat lvl1) (concat lvl2)

-- tail should be: last
-- 85,53-56
