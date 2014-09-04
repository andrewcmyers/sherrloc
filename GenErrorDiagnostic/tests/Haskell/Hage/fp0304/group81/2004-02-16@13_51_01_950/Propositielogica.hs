module Propositielogica where

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
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

evalueer                 :: Prop -> Bedeling -> Bool
evalueer (Var p          ) bed = elemBy eqString p bed
evalueer (Niet prop      ) bed = not (evalueer prop bed)
evalueer (En props       ) bed = and (map (\x -> evalueer x bed) props)
evalueer (Of props       ) bed = or  (map (\x -> evalueer x bed) props)
evalueer (prop1 :-> prop2) bed = evalueer (Of [Niet prop1 , prop2]) bed
evalueer (Bool bool      ) _   = bool



vervulbaar                 :: Prop -> [Bedeling]
vervulbaar (Var p          ) = [ [ p ] ]
vervulbaar (Niet prop      ) = vervulbaar prop
vervulbaar (En props       ) = concatMap vervulbaar props
vervulbaar (Of props       ) = concatMap vervulbaar props
vervulbaar (prop1 :-> prop2) = (vervulbaar prop1) ++ (vervulbaar prop2)
vervulbaar (Bool _         ) = [ ]

alleVars :: Prop -> [String]
alleVars (Var p      ) = [p]
alleVars (Niet prop  ) = alleVars prop
alleVars (En props   ) = map alleVars props
















toon                   :: Prop -> String
toon (Var p)           = p
toon (Niet prop)       = "!" ++ toon prop
toon (En props)        = "(" ++ concat (insert " /\\ " (map toon props)) ++ ")"
toon (Of props)        = "(" ++ concat (insert " \\/ " (map toon props)) ++ ")"
toon (prop1 :-> prop2) = "(" ++ toon prop1 ++ " -> " ++ toon prop2 ++ ")"
toon (Bool bool)       | eqBool True bool = "True"
                       | otherwise        = "False"

insert          ::  a -> [a] -> [a]
insert _ []      = []
insert op (x:xs) = x : op : insert op xs

-- map should be: concatMap
-- 41,26-28   41,26-43
