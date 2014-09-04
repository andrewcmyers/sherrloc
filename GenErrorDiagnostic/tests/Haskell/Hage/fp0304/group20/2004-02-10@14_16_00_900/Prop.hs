module Prop where

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
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
evalueer (Bool b) _          = b
evalueer (Var v) a           = elemBy eqString v a
evalueer (p :-> q) a         = implicatie (evalueer p a) (evalueer q a)
evalueer (Niet p) a          = not (evalueer p a)
evalueer (En ps) a           = foldr (&&) True (evalueerlijst ps a)
evalueer (Of ps) a           = foldr (||) False (evalueerlijst ps a)

implicatie :: Bool -> Bool -> Bool
implicatie b1 b2 | b1&& (not b2) = False
                 | otherwise = True

evalueerlijst :: [Prop] -> Bedeling -> [Bool]
evalueerlijst ps a = [evalueer p a | p <- ps]


vervulbaar :: Prop -> [Bedeling]
vervulbaar (Bool _)   = []
vervulbaar (Var v)    = [[v]]


vervulbaar (En ps)    = concat (map (variabelen) ps)
vervulbaar (Of ps)    = map (variabelen) ps

variabelen :: Prop -> [String]
variabelen (Bool _)   = []
variabelen (Var v)    = [v]
variabelen (Niet q)   = variabelen q
variabelen (En q)     = concat [variabelen p | p <- q]
variabelen (Of q)     = concat [variabelen p | p <- q]
variabelen (p :-> q)  = variabelen p ++ variabelen q

-- should delete concat
-- 40,25-52   40,25-30
