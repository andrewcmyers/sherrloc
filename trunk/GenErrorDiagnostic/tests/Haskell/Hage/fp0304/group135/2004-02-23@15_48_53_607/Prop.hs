module Prop where

import Data.List

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

data Prop = En [Prop]
          | Of [Prop]
          | Niet Prop
          | Prop :-> Prop
          | Var String
          | Bool Bool

type Bedeling = [String]




evalueer :: Prop -> Bedeling -> Bool
evalueer (En xs) b   = (and.map f) xs
                       where f p = evalueer p b
evalueer (Of xs) b   = (or.map f) xs
                       where f p = evalueer p b
evalueer (Niet p) b  = (not.evalueer p) b
evalueer (p :-> q) b = evalueer (Of [p, Niet q]) b
evalueer (Var p)   b = elemBy eqString p b
evalueer (Bool p)  _ = p




vervulbaar :: Prop -> [Bedeling]
vervulbaar p = filter (evalueer p) (alleBedelingen p)

alleBedelingen :: Prop -> [[a]]
alleBedelingen = subs.nubBy eqString.alleVars

alleVars :: Prop -> Bedeling
alleVars (En xs)   = concatMap alleVars xs
alleVars (Of xs)   = concatMap alleVars xs
alleVars (Niet p)  = alleVars p
alleVars (p :-> q) = alleVars p ++ alleVars q
alleVars (Var p)   = [p]
alleVars (Bool _)  = []

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
              where subsxs = subs xs




tautologie :: Prop -> Bool
tautologie p = (and.map (evalueer p)) (alleBedelingen p)




contradictie :: Prop -> Bool
contradictie p = (and.map (not.evalueer p)) (alleBedelingen p)




toon :: Prop -> String
toon p = toonVereenvoudigd p 4

toonVereenvoudigd :: Prop -> Int -> String
toonVereenvoudigd (En []) _                 = "T"
toonVereenvoudigd (En (x:xs)) _             = foldl f (toonVereenvoudigd x 1) xs
                                              where f s p = s ++ " /\\ " ++ toonVereenvoudigd p 1
toonVereenvoudigd (Of []) _                 = "F"
toonVereenvoudigd (Of (x:xs)) i | i < 2     = foldl f (toonVereenvoudigd x i) xs
                                | otherwise = foldl g (toonVereenvoudigd x 2) xs
                                              where f s p = "(" ++ s ++ " \\/ " ++ toonVereenvoudigd p i ++ ")"
                                                    g s p = s ++ " \\/ " ++ toonVereenvoudigd p 2
toonVereenvoudigd (Niet (Var p)) _          = "!" ++ p
toonVereenvoudigd (Niet (Bool p)) i         = toonVereenvoudigd (Bool (not p)) i
toonVereenvoudigd (Niet p) i                = "!(" ++ toonVereenvoudigd p i ++ ")"
toonVereenvoudigd (p :-> q) i   | i < 3     = "(" ++ toonVereenvoudigd p i ++ " -> " ++ toonVereenvoudigd q i ++ ")"
                                | otherwise = toonVereenvoudigd p 3 ++ " -> " ++ toonVereenvoudigd q 3
toonVereenvoudigd (Var p) _                 = p
toonVereenvoudigd (Bool p) _    | p         = "T"
                                | otherwise = "F"




equivalent :: Prop -> Prop -> Bool
equivalent p q = eqList eqBool (map (evalueer p) a) (map (evalueer q) a)
                 where a = alleBedelingen (En [p, q])

-- signature should be: Prop -> [Bedeling]
-- 42,1-14   42,19-31   42,27-31
