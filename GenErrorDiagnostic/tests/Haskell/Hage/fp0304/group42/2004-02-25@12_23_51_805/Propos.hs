module Propos where
import Data.List

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
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined
ord :: Char -> Int
ord = undefined

type Bedeling = [String]















evalueer :: Prop -> Bedeling -> Bool
evalueer (Bool b)   _  = b
evalueer (Var s)    xs = elemBy eqString s xs
evalueer (Niet p)   xs = not (evalueer p xs)
evalueer (En p)     xs = all (`evalueer` xs) p
evalueer (Of p)     xs = any (`evalueer` xs) p
evalueer (p :-> q)  xs = evalueer (Of [Niet p, q]) xs

getPropLetters :: Prop -> [String]
getPropLetters prop = nubBy eqString (getPropLetters' prop)
               where getPropLetters' (Bool _) = []
                     getPropLetters' (En px) = concatMap getPropLetters' px
                     getPropLetters' (Of px) = concatMap getPropLetters' px
                     getPropLetters' (Niet p) = getPropLetters' p
                     getPropLetters' (p :-> q) = getPropLetters' p ++ getPropLetters' q
                     getPropLetters' (Var s) = [s]


subs :: [a] -> [[a]]
subs [] = [ [] ]
subs (x:xs) = map (x:) (subs xs) ++ subs xs


vervulbaar :: Prop -> [Bedeling]
vervulbaar p = filter (evalueer p) (subs (getPropLetters p))


tautologie :: Prop -> Bool
tautologie p = and (map (evalueer p) (subs (getPropLetters p)))


contradictie :: Prop -> Bool
contradictie p = null (vervulbaar p)














toon :: Prop -> String
toon prop = snd(maakTup prop)
          where maakTup (Bool _) = (0, "")
                maakTup (Var s) = (0, s)
                maakTup (Niet p) = (1, "!" ++ (bepaalHaken 1 . maakTup) p)
                maakTup (En px) = (2, implode " /\\ " (map (bepaalHaken 2 . maakTup) px))
                maakTup (Of px) = (3, implode " \\/ " (map (bepaalHaken 3 . maakTup) px))
                maakTup (p1 :-> p2) = (4, (bepaalHaken 4 . maakTup) p1 ++ " -> " ++ (bepaalHaken 4 . maakTup) p2)


bepaalHaken :: Int -> (Int, String) -> String
bepaalHaken a (b,s) | b > a = "(" ++ s ++ ")"
                    | otherwise = s



implode :: String -> [String] -> String
implode glue list = reverse (drop (length glue) (reverse (implode' list)))
               where implode' (x:xs) = x ++ glue ++ (implode' xs)
                     implode' [] = ""



eqList2 :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList2 _ [] [] = True
eqList2 f (x:xs) ys | elemBy f x ys = eqList2 f xs (deleteBy f x ys)
                    | otherwise = False
eqList2 _ [] _ = False


equivalent :: Prop -> Prop -> Bool
equivalent p1 p2 = eqList2 eqBool (map (evalueer p1) allPropLettersSubs) (map (evalueer p2) allPropLettersSubs)
                 where allPropLettersSubs = subs (nubBy eqString ((getPropLetters p1) ++ (getPropLetters p2)))


deMorgan :: Prop -> Prop
deMorgan (Niet (En [p, q])) = deMorgan (Of [(Niet p), (Niet q)])
deMorgan (Niet (Of [p, q])) = deMorgan (En [(Niet p), (Niet q)])
deMorgan (Niet p) = Niet (deMorgan p)
deMorgan (En px) = En (map deMorgan px)
deMorgan (Of px) = Of (map deMorgan px)
deMorgan (Bool b) = Bool b
deMorgan (Var s) = Var s
deMorgan (p :-> q) = (deMorgan p) :-> (deMorgan q)


deMorgan2 :: Prop -> Prop
deMorgan2 (Of [(Niet p), (Niet q)]) = deMorgan2 (Niet (En [p, q]))
deMorgan2 (En [(Niet p), (Niet q)]) = deMorgan2 (Niet (Of [p, q]))
deMorgan2 (Niet p) = Niet (deMorgan2 p)
deMorgan2 (En px) = En (map deMorgan2 px)
deMorgan2 (Of px) = Of (map deMorgan2 px)
deMorgan2 (Bool b) = Bool b
deMorgan2 (Var s) = Var s
deMorgan2 (p :-> q) = (deMorgan2 p) :-> (deMorgan2 q)


getPropLetters1 :: String -> [String]
getPropLetters1 (p:px) | ord(p) >= 97 && ord(p) <= 122 = [p] : getPropLetters1 px
                       | otherwise = getPropLetters1 px
getPropLetters1 [] = []

isPropLetter1 :: String -> String -> Bool
isPropLetter1 a letters = elemBy eqString [a] (getPropLetters1 letters)

ontleed :: String -> Prop
ontleed (x:xs) | isPropLetter1 [x] (x:xs) = Var [x]
ontleed [] = Bool False

-- [a] should be: a
-- 147,43-45   147,44-44
