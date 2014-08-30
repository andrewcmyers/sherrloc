module Logica where

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

type
   Bedeling = [String]

evalueer :: Prop -> Bedeling -> Bool
evalueer (Bool b) _               =  b
evalueer (Var v) bedeling         = elemBy eqString v bedeling
evalueer (Niet p) bedeling        = not (evalueer p bedeling)
evalueer (En p) bedeling          = and [evalueer p1 bedeling | p1 <- p]
evalueer (Of p) bedeling          = or [evalueer p1 bedeling | p1 <- p]
evalueer (p1 :-> p2) bedeling     = or [(evalueer (Niet p1) bedeling), (evalueer p2 bedeling)]

variabelen :: Prop -> [String]
variabelen (Var v)     = [v]
variabelen (Bool _)    = []
variabelen (Niet p)    = variabelen p
variabelen (En p)      = concat [variabelen p1 | p1 <- p]
variabelen (Of p)      = concat [variabelen p1 | p1 <- p]
variabelen (p1 :-> p2) = variabelen p1 ++ variabelen p2

deelRij :: [String] -> [[String]]
deelRij lijst | null lijst = [[]]
              | otherwise = (deelRij (tail lijst)) ++ map (head lijst:)(deelRij (tail lijst))

vervulbaar :: Prop -> [Bedeling]
vervulbaar p = map (evalueer p) (deelRij (variabelen p))

-- map should be: filter
-- 40,16-56
