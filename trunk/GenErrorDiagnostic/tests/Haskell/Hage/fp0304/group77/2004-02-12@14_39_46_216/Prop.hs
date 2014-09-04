module Prop where

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
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqChar      :: Char -> Char -> Bool
eqChar = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

evalueer :: Prop -> Bedeling -> Bool
evalueer (Bool b) _          = b
evalueer (Var v) a           = elemBy eqString v a
evalueer (p :-> q) a         = implicatie (evalueer p a) (evalueer q a)
evalueer (Niet p) a          = not (evalueer p a)
evalueer (En ps) a           = foldr (&&) True (evalueerlijst ps a)
evalueer (Of ps) a           = foldr (||) False (evalueerlijst ps a)

implicatie :: Bool -> Bool -> Bool
implicatie b1 b2 | b1 && (not b2) = False
                 | otherwise = True

evalueerlijst :: [Prop] -> Bedeling -> [Bool]
evalueerlijst ps a           = [evalueer p a | p <- ps]


vervulbaar :: Prop -> [Bedeling]
vervulbaar (Bool _)          = []
vervulbaar (Var v)           = [[v]]
vervulbaar (Niet q)          = filterBedelingen (Niet q)
vervulbaar (p :-> q)         = filterBedelingen (p :-> q)
vervulbaar (En ps)           = filterBedelingen (En ps)
vervulbaar (Of ps)           = filterBedelingen (Of ps)

filterBedelingen :: Prop -> [Bedeling]
filterBedelingen prop        = filter (evalueer prop) (subs (variabelen prop))

subs :: [a] -> [[a]]
subs []                      = [[]]
subs (x:xs)                  = map (x:) (subs xs) ++ subsxs
                             where subsxs = subs xs

variabelen :: Prop -> [String]
variabelen (Bool _)          = []
variabelen (Var v)           = [v]
variabelen (Niet q)          = variabelen q
variabelen (En q)            = concat [variabelen p | p <- q]
variabelen (Of q)            = concat [variabelen p | p <- q]
variabelen (p :-> q)         = variabelen p ++ variabelen q


tautologie :: Prop -> Bool
tautologie p                 = null (vervulbaar (Niet p))


contradictie :: Prop -> Bool
contradictie p               = null (vervulbaar p)


toon :: Prop -> String
toon (Bool True)             = "T"
toon (Bool False)            = "F"
toon (Var v)                 = v
toon (Niet q)                = "!" ++ (toon q)
toon (p :-> q)               = (toon p) ++ "->" ++ (toon q)
toon (En ps)                 = "(" ++ (toonEnOf (En ps) "/\\") ++ ")"
toon (Of ps)                 = "(" ++ (toonEnOf (Of ps) "\\/") ++ ")"

toonEnOf :: [Prop] -> String -> String
toonEnOf []     string       = ""
toonEnOf (x:xs) string       | null xs   = (toon x)
                             | otherwise = (toon x) ++ string ++ (toonEnOf xs string)


equivalent :: Prop -> Prop -> Bool
equivalent (Bool a) (Bool b) = eqBool a b
equivalent (Bool True) a     = tautologie a
equivalent a (Bool True)     = tautologie a
equivalent (Bool False) a    = contradictie a
equivalent a (Bool False)    = contradictie a
equivalent prop1 prop2       = eqList (eqList (eqString)) (vervulbaar prop1) (vervulbaar prop2)

-- (En ps) and (Of ps) should be ps
-- 79,50-54   80,50-54
