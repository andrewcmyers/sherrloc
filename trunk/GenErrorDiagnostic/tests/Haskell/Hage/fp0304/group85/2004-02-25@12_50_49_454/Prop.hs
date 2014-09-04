module Prop where

import Data.List

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
ordString :: String -> String -> Ordering
ordString = undefined

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool
  | Oml Prop

type Bedeling = [String]








evalueer :: Prop -> Bedeling -> Bool


evalueer (En props) bedeling  =  and (map (flip evalueer bedeling) props)

evalueer (Of props) bedeling  =  or  (map (flip evalueer bedeling) props)

evalueer (Niet prop) bedeling = not (evalueer prop bedeling)

evalueer (prop1 :-> prop2) bedeling = evalueer (Of [(Niet prop1), prop2]) bedeling

evalueer (Bool boolean) _     = boolean
evalueer (Var string) (l:ls)  = eqString string l || evalueer (Var string) ls

evalueer _ []                 = False
evalueer _ _                  = error "Prop.evalueer: Do only use proper operators"







vervulbaar :: Prop -> [Bedeling]
vervulbaar prop = filter (evalueer prop) ((subsets.alleVars) prop)

alleVars :: Prop -> Bedeling

alleVars (En props)   = rmvDouble (concatMap alleVars props)
alleVars (Of props)   = rmvDouble (concatMap alleVars props)
alleVars (Niet prop)  = alleVars prop
alleVars (prop1 :-> prop2) = rmvDouble (alleVars prop1 ++ alleVars prop2)
alleVars (Bool _)     = []
alleVars (Var string) = [string]
alleVars _            = error "Prop.alleVars: Do only use proper operators!"

rmvDouble :: Bedeling -> Bedeling

rmvDouble (x:xs) | elemBy eqString x xs = rmvDouble xs
                 | otherwise = x:(rmvDouble xs)
rmvDouble []     = []

subsets :: [a] -> [[a]]

subsets []     = [[]]
subsets (x:xs) = (map (x:) subxs) ++ subxs
        where subxs = subsets xs







tautologie :: Prop -> Bool
tautologie prop = and (mogUitkomsten prop)

mogUitkomsten      :: Prop -> [Bool]

mogUitkomsten prop = map (evalueer prop) ((subsets.alleVars) prop)




contradictie :: Prop -> Bool
contradictie prop = and (map not (mogUitkomsten prop))








toon :: Prop -> String
toon p@(Of props)        = plaatsOperator " \\/ " (map (haakjes p) props)
toon p@(En props)        = plaatsOperator " /\\ " (map (haakjes p) props)
toon p@(prop1 :-> prop2) = plaatsOperator " -> "  (map (haakjes p) [(Oml prop1), prop2])

toon p@(Niet prop)       = "!" ++ (haakjes p prop)
toon (Var variable)      = variable
toon (Bool bool)         | bool      = "True"
                         | otherwise = "False"
toon (Oml prop)          = toon prop

haakjes :: Prop -> Prop -> String
haakjes prop1 prop2       | plaatsHaakjes (prioriteit prop1) (prioriteit prop2) = "(" ++ toon prop2 ++ ")"
                          | otherwise = toon prop2



plaatsOperator :: String -> [String] -> String
plaatsOperator op (x: xs) = x ++ concatMap (op ++) xs
plaatsOperator _ []       = []


plaatsHaakjes :: Int -> Int -> Bool

plaatsHaakjes prio1 prio2 | prio1 < prio2 = False
                          | prio1 > prio2 = True
                          | otherwise     = False


prioriteit :: Prop -> Int

prioriteit ( Oml prop) = (prioriteit prop)-1
prioriteit ( _ :-> _ ) = 0
prioriteit ( Of _ )    = 1
prioriteit ( En _ )    = 2
prioriteit ( Niet _ )  = 3
prioriteit _           = 9












waarheidsTabel :: [Bedeling] -> Prop -> [Bool]
waarheidsTabel bedeling prop = map (evalueer prop) bedeling


bedelingen :: Prop -> Prop -> Bedeling

bedelingen prop1 prop2 | eqList eqString (aVar bedeling1) (aVar bedeling2) = bedeling1
                       | otherwise = error "Prop.bedelingen: Propositions have different variables"
                           where
                             aVar prop = (sortBy ordString (alleVars prop))
                             bedeling1 = aVar prop1
                             bedeling2 = aVar prop2

-- should be prop1 and prop2 respectively
-- 161,48-56   161,65-73
