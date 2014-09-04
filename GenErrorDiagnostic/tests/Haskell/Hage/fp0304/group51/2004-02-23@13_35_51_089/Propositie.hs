module Propositie where

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqChar      :: Char -> Char -> Bool
eqChar = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

data Prop = En [Prop] | Of [Prop] | Niet Prop | Prop :-> Prop | Var String | Bool Bool

type Bedeling = [String]

evalueer :: Prop -> Bedeling -> Bool
evalueer (En (xs)) b = and $ map (`evalueer` b) xs
evalueer (Of (xs)) b = or $ map (`evalueer` b) xs
evalueer (Niet a) ys = (not (evalueer a ys))
evalueer (a :-> b) ys = ( evalueer a ys || (not (evalueer b ys)))


evalueer (Var a) xs = foldr (||) False (map eqString xs)
evalueer (Bool a) _ = a
evalueer _ [] = False

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
          where subsxs = subs xs

verwijderDubbele :: [String] -> [String]
verwijderDubbele [] = []
verwijderDubbele (x:xs) | elemBy eqString x xs = verwijderDubbele xs
                        | otherwise        = x : verwijderDubbele xs

verwLeeg :: [[String]] -> [[String]]
verwLeeg [] = []
verwLeeg (xs:xss) | eqList eqString xs [] = verwLeeg xss
                  | otherwise = xs : verwLeeg xss

variabelen :: Prop -> Bedeling
variabelen (En (xs)) = verwijderDubbele (concatMap variabelen xs)
variabelen (Of (xs)) = verwijderDubbele (concatMap variabelen xs)
variabelen (Niet xs)  = variabelen xs
variabelen (a :-> b) = verwijderDubbele (variabelen a ++ variabelen b)
variabelen (Var a) = [a]
variabelen _ = []


vervulbaar :: Prop -> [Bedeling]
vervulbaar p = verwLeeg [x | x <- subs (variabelen p), evalueer p x] ++ [[]]


tautologie :: Prop -> Bool
tautologie p = [ x | x <- subs (variabelen p), not (evalueer p x)] `eqStrLists` []


eqStrLists :: [[String]] -> [[String]] -> Bool
eqStrLists [[]] [[]] = True
eqStrLists _ [[]] = False
eqStrLists [[]] _ = False
eqStrLists lvl1 lvl2 = eqList eqString (concat lvl1) (concat lvl2)


contradictie :: Prop -> Bool
contradictie a = (vervulbaar a) `eqStrLists` [[]]

toon :: Prop -> String
toon (En xs)       = "("++ zetTussen (map toon xs) " /\\ " ++ ")"
toon (Of xs)       = "("++ zetTussen (map toon xs) " \\/ " ++ ")"
toon (Niet a)      = "!("++toon a++")"
toon (xs :-> ys)   = "("++toon xs ++ " -> " ++ toon ys ++")"
toon (Var a)       = a
toon (Bool a)      | a `eqBool` False = "F"
                   | True = "T"

zetTussen :: [String] -> String -> String
zetTussen xs str= concatMap (++ str) (init xs) ++ last xs



equivalentie :: Prop -> Prop -> Bool
equivalentie a b | (vervulbaar a) `eqStrLists` (vervulbaar b) = True
                 | otherwise = False


niet :: Prop -> Prop
niet (En xs)   = Of (niets xs)
niet (Of xs)   = En (niets xs)
niet (Niet a)  = a
niet (a :-> b) = En (niets [a, (niet b)])
niet (Var a)   = Niet (Var a)
niet (Bool a)  = Bool (not a)

niets :: [Prop] -> [Prop]
niets [] = []
niets (x:xs) = (niet x): (niets xs)

deMorgan :: Prop -> Prop
deMorgan (Niet ( En xs )) = (Of (niets xs))
deMorgan (Niet ( Of xs )) = (En (niets xs))
deMorgan (Niet a)   = deMorgan (niet a)
deMorgan (En xs) = En (map deMorgan xs)
deMorgan (Of xs) = Of (map deMorgan xs)
deMorgan (a :-> b)  = (deMorgan a) :-> (deMorgan b)
deMorgan (Var a)    = Var a
deMorgan (Bool a)   = Bool a

-- eqString should be: (eqString a)
-- 25,45-52
