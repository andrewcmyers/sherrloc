module Proposities1 where

import Data.List hiding (insert)

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




type Bedeling = [String]











evalueer :: Prop -> Bedeling -> Bool
evalueer (Bool bool) _         = bool
evalueer (Var var) bed         = elemBy eqString var bed
evalueer (En props) bed        = and (map (\x -> evalueer x bed) props)
evalueer (Of props) bed        = or (map (\x -> evalueer x bed) props)
evalueer (Niet prop) bed       = not (evalueer prop bed)
evalueer (prop1 :-> prop2) bed = (not (evalueer prop1 bed)) || (evalueer prop2 bed)











vervulbaar :: Prop -> [Bedeling]
vervulbaar prop = filter (\x -> evalueer prop x) (alleBedelingen prop)










alleBedelingen :: Prop -> [Bedeling]
alleBedelingen prop = (subs.variabele) prop








subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
              where subsxs = subs xs






variabele :: Prop->[String]
variabele prop = nubBy eqString (variabele2 prop)
    where
        variabele2 (Bool _)          = []
        variabele2 (Var var)         = [var]
        variabele2 (En props)        = concatMap variabele2 props
        variabele2 (Of props)        = concatMap variabele2 props
        variabele2 (Niet p)          = variabele2 p
        variabele2 (prop1 :-> prop2) = (variabele2 prop1)++(variabele2 prop2)


















tautologie :: Prop -> Bool
tautologie prop = and (map (evalueer prop) (alleBedelingen prop))









contradictie :: Prop -> Bool
contradictie prop = null (vervulbaar prop)























































toon :: Prop -> String
toon prop = case prop of
  Var var         -> var
  Bool True       -> "True"
  Bool False      -> "False"
  En props        -> concat (insert " /\\ " (testPrioriteit (prioriteit (En props)) props))
  Of props        -> concat (insert " \\/ " (testPrioriteit (prioriteit (Of props)) props))
  prop1 :-> prop2 -> concat ((rechtsAss (prioriteit (prop1 :-> prop2)) [prop1]) ++ [" -> "] ++ (testPrioriteit (prioriteit (prop1 :-> prop2)) [prop2]))

  Niet p          -> '!' : unwords (testPrioriteit (prioriteit (Niet p)) [p])




















testPrioriteit :: Int -> [Prop] -> [String]
testPrioriteit _ [] = []
testPrioriteit x (p:ps) | x > prioriteit p = concat (["("] ++ [(toon p)] ++  [")"]) : (testPrioriteit x ps)
                        | otherwise = (toon p) : (testPrioriteit x ps)

rechtsAss :: Int -> [Prop] -> [String]
rechtsAss x prop | (x==1) && ((prioriteit prop)==1) = (["("] ++ [(toon prop)] ++  [")"])
                 | otherwise = testPrioriteit x prop

prioriteit :: Prop -> Int
prioriteit (Var _)   = 5
prioriteit (Bool _)  = 5
prioriteit (Niet _)  = 4
prioriteit (En _)    = 3
prioriteit (Of _)    = 2
prioriteit (_ :-> _) = 1


























insert :: String -> [String] -> [String]
insert symbool lijst    = init $ foldr insSymbol [] lijst
    where
        insSymbol e xs  = e : symbool : xs











equivalent :: Prop -> Prop -> Bool
equivalent prop1 prop2 = tautologie (En [(prop1 :-> prop2) , (prop2 :-> prop1)])










deMorgan :: Prop -> Prop
deMorgan (Var var)         = Var var
deMorgan (Bool bool)       = Bool bool
deMorgan (En props)        = En (map deMorgan props)
deMorgan (Of props)        = Of (map deMorgan props)
deMorgan (Niet (Of props)) = deMorgan (En (map Niet props))
deMorgan (Niet (En props)) = deMorgan (Of (map Niet props))
deMorgan (prop1 :-> prop2) = (deMorgan prop1) :-> (deMorgan prop2)
deMorgan (Niet prop)       = Niet (deMorgan prop)

-- prop should be [prop] at both line 214 and 215
-- 214,13-16   215,49-52
