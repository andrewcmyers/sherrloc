module Logica where

import ParserLibrary
import Data.List
showBool       :: Bool -> String
showBool = undefined
type Bedeling = [String]
data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

evalueer:: Prop -> Bedeling -> Bool
evalueer (Bool a) _          = a
evalueer (Var a) bed         = elemBy eqString a bed
evalueer (a :-> b) bed       = or ((not (evalueer a bed)) : evalueer b bed: [])
evalueer (Niet a) bed        = not (evalueer a bed)
evalueer (Of a) bed          = or [evalueer x bed|x<- a]
evalueer (En a) bed          = and [evalueer x bed|x<- a]

vervulbaar:: Prop -> [Bedeling]
vervulbaar a                 = [bedeling | bedeling<- (subs (nubBy eqString (alleVar a))), evalueer a bedeling]

alleVar :: Prop -> Bedeling
alleVar a                    = nubBy eqString (alleVar2 a)

alleVar2 :: Prop -> Bedeling
alleVar2 (Bool _)            = [""]
alleVar2 (Var a )            = [a]
alleVar2 (a :-> b)           = (concat[(alleVar a),(alleVar b)])
alleVar2 (Niet a)            = alleVar a
alleVar2 (Of a)              = concat [alleVar x | x<-a]
alleVar2 (En a)              = concat [alleVar x | x<-a]

tautologie:: Prop -> Bool
tautologie a                 = and [evalueer a x | x <- subs (alleVar a)]

contradictie:: Prop -> Bool
contradictie a               = and [not (evalueer a x) | x <- subs (alleVar a)]

equivalent:: Prop -> Prop -> Bool
equivalent  a b              = eqList (eqBool) lijst1 lijst2
                             where mogBedeling       = subs (alleVar a)++ subs (alleVar b)
                                   lijst1            = [evalueer a x | x <- mogBedeling]
                                   lijst2            = [evalueer b x | x <- mogBedeling]

toon:: Prop -> String

toon (Bool a)                = showBool a
toon (Var a )                = a



toon (Niet (Var a))          =  "!" ++  a
toon (Niet (Bool a))         = showBool(not a)
toon (Niet (En a))           = "!" ++ "(" ++ (toon (En a)) ++ ")"
toon (Niet (Of a))           = "!" ++ "(" ++ (toon (Of a)) ++ ")"
toon (Niet c@(_ :-> _))      = "!" ++ "(" ++ (toon c) ++ ")"
toon (Niet (Niet a))         = toon a

toon (En [Of a])             = "(" ++  (toon (Of a)) ++ ")"
toon (En [(c@(_:->_))])      = "(" ++  (toon c) ++ ")"
toon (En [a])                = toon a
toon (En ((Of a):as))        = "(" ++  (toon (Of a)) ++ ")" ++ "  /\\  " ++ (toon (En as))
toon (En (c@(_:->_):as))     = "(" ++ (toon c) ++ ")"++ "  /\\  " ++ (toon (En as))
toon (En (a:as))             = toon a ++ "  /\\  " ++ (toon (En as))

toon (Of [(c@(_:->_))])      = "(" ++  (toon c) ++ ")"
toon (Of [a])                = toon a
toon (Of (c@(_:->_):as))     = "(" ++ (toon c) ++ ")"++ "  \\/  " ++ (toon (Of as))
toon (Of (a:as))             = toon a ++  "  \\/  " ++ (toon (Of as))

toon (a@(_:->_):->b)           = "(" ++ (toon a) ++ ")" ++ "   ->   " ++ (toon b)
toon (a :-> b)               = (toon a )++ "   ->   " ++ (toon b)

toon _                       = ""

deMorgan:: Prop -> Prop
deMorgan (En a)              = (En (map deMorgan a))
deMorgan (Of a)              = (Of (map deMorgan a))
deMorgan (Niet (En a))       = deMorgan (Of (map Niet a))
deMorgan (Niet (Of a))       = deMorgan (En (map Niet a))
deMorgan (a :-> b)           = (deMorgan a)  :-> (deMorgan b)
deMorgan a                   = a

subs:: [a] -> [[a]]
subs []                      = [[]]
subs (x:xs)                  = map (x:) subsxs ++ subsxs
                             where subsxs = subs xs



prop:: String -> [((Char, a), String)]
prop = letter '!' `sequ` prop `doe` (\(_,a) -> Niet a)




constants:: Parser Prop
constants  = bool `doe` (\e1 -> Bool e1)`orelse` variabele `doe` (\e1 -> Var e1)

-- signature should be: String -> [((Char, Prop), String)]
-- 103,1-4   103,8-38   103,18-38
