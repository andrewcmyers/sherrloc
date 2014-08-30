module Logica2 where
import Data.List

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

type Bedeling = [String]





evalueer :: Prop -> Bedeling -> Bool
evalueer (En ps)     b = and $ map (\x->evalueer x b) ps
evalueer (Of ps)     b = or  $ map (\x->evalueer x b) ps
evalueer (Niet p)    b = not (evalueer p b)
evalueer (p1 :-> p2) b | eqBool (evalueer p1 b) True && eqBool (evalueer p2 b) False = False
                       | otherwise                                                   = True
evalueer (Var s)     b = elemBy eqString s b
evalueer (Bool bool) _ = bool





vervulbaar :: Prop -> [Bedeling]
vervulbaar p = [x | x<-alleSubs p, evalueer p x]

alleSubs :: Prop -> [Bedeling]
alleSubs p = subs $ nubBy eqString $ concat $ alleProps p

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
              where subsxs = subs xs

alleProps :: Prop -> [Bedeling]
alleProps (En ps)     = concat $ map alleProps ps
alleProps (Of ps)     = concat $ map alleProps ps
alleProps (Niet p)    = alleProps p
alleProps (p1 :-> p2) = alleProps p1 ++ alleProps p2
alleProps (Var s)     = [[s]]
alleProps (Bool _)    = []





tautologie :: Prop -> Bool
tautologie p = and $ map (evalueer p) (alleSubs p)





contradictie :: Prop -> Bool
contradictie p = and $ map (not.(evalueer p)) (alleSubs p)





toon :: Prop -> String
toon (En (p:ps)) = toon p ++ map ( ("/\\ "++).toon ) ps
toon (Of (p:ps)) = toon p ++ map ( ("\\/ "++).toon ) ps
toon (Niet p)    = "!" ++ toon p
toon (p1 :-> p2) = toon p1 ++ "-> " ++ toon p2
toon (Var s)     = s ++ " "
toon (Bool bool) | bool      = "True "
                 | otherwise = "False "












deMorgan :: Prop -> Prop
deMorgan (Niet (En [p1, p2])) = Of [ Niet (deMorgan p1), Niet (deMorgan p2) ]
deMorgan (Niet (Of [p1, p2])) = En [ Niet (deMorgan p1), Niet (deMorgan p2) ]
deMorgan (En ps)              = En (map deMorgan ps)
deMorgan (Of ps)              = Of (map deMorgan ps)
deMorgan (Niet p)             = Niet (deMorgan p)
deMorgan (p1 :-> p2)          = deMorgan p1 :-> deMorgan p2
deMorgan (Var s)              = Var s
deMorgan (Bool bool)          = Bool bool

-- map should be concatMap
-- 76,30-55   77,30-55
