module Propositie (module List, module Propositie) where
import Data.List

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

type Bedeling = [String]

eqString      :: String -> String -> Bool 
eqString = undefined
eqChar      :: Char -> Char -> Bool 
eqChar = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined
notElemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
notElemBy = undefined
isSpace :: Char -> Bool
isSpace = undefined






evalueer :: Prop -> Bedeling -> Bool
evalueer (Bool b) _  = b
evalueer (Var _)  [] = False
evalueer (Var p)  (x:xs) | eqString p x = True
                         | otherwise    = evalueer (Var p) xs
evalueer (Niet p) xs  = not(evalueer p xs)
evalueer (En ps)  xs  = and(map f ps)
           where f p  = evalueer p xs
evalueer (Of ps) xs   = or(map f ps)
           where f p  = evalueer p xs
evalueer (p :-> q) xs = evalueer (Of [Niet (p), q]) xs











proposities :: Prop -> [String]
proposities (Bool _)  = []
proposities (Var p)   = [p]
proposities (Niet p)  = proposities p
proposities (En ps)   = concatMap proposities ps
proposities (Of ps)   = concatMap proposities ps
proposities (p :-> q) = proposities (Of [p, q])


varLijst        :: [String] -> [String]
varLijst []     = []
varLijst (x:xs) | notElemBy (eqString) x xs = [x] ++ varLijst xs
                | otherwise                 = varLijst xs



subs        :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
            where subsxs = subs xs


vervulbaar   :: Prop -> [Bedeling]
vervulbaar p = filter (evalueer p) (subs (varLijst (proposities p)))







tautologie   :: Prop -> Bool
tautologie p = length (vervulbaar (Niet p)) == 0







contradictie   :: Prop -> Bool
contradictie p = length (vervulbaar p) == 0









toon :: Prop -> String
toon p = tonen p 4




tonen :: Prop -> Int -> String
tonen (Bool b) _ | b         = "True"
                 | otherwise = "False"
tonen (Var p) _  = p
tonen (Niet p) level | level < 1 = "!(" ++ tonen p 1 ++ ")"
                     | otherwise = "!" ++ tonen p 1
tonen (En ps) level  | null (ps)      = ""
                     | length ps == 1 = tonen (head ps) 2
                     | level < 2      = "(" ++ tonen (head ps) 2 ++ "/\\" ++ tonen (En (tail ps)) 2 ++ ")"
                     | otherwise      = tonen (head ps) 2 ++ "/\\" ++ tonen (En (tail ps)) 2
tonen (Of ps) level  | null (ps)      = ""
                     | length ps == 1 = tonen (head ps) 3
                     | level < 3      = "(" ++ tonen (head ps) 3 ++ "\\/" ++ tonen (Of (tail ps)) 3 ++ ")"
                     | otherwise      = tonen (head ps) 3 ++ "\\/" ++ tonen (Of (tail ps)) 3
tonen (p :-> q) _    = tonen p 4 ++ "->" ++ tonen q 4











equivalent     :: Prop -> Prop -> Bool
equivalent p q = null (filter (geenEquivalentie p q) (subs (varLijst (proposities p ++ proposities q))))



geenEquivalentie       :: Prop -> Prop -> Bedeling -> Bool
geenEquivalentie p q b = not (eqBool (evalueer p b) (evalueer q b))







deMorgan           :: Prop -> Prop
deMorgan (Bool b)  = Bool b
deMorgan (Var p)   = Var p
deMorgan (En ps)   = En (map deMorgan ps)
deMorgan (Of ps)   = Of (map deMorgan ps)
deMorgan (Niet (En [p, q])) = Of [deMorgan (Niet p), deMorgan (Niet q)]
deMorgan (Niet (Of [p, q])) = En [deMorgan (Niet p), deMorgan (Niet q)]
deMorgan (Niet p)  = Niet (deMorgan p)
deMorgan (p :-> q) = deMorgan p :-> deMorgan q













checkChar :: String -> String -> Int -> Int -> Bool
checkChar s t i j | and [j<length t, i+j<length s] = not (eqChar (s!!(i+j)) (t!!j))
                  | otherwise                      = True



zoekString :: String -> String -> Int -> Int -> Int
zoekString s t i level | i>=length s                      = -1
                       | eqChar (s!!i) '('                = zoekString s t (i+1) (level+1)
                       | and [eqChar (s!!i) ')', level>0] = zoekString s t (i+1) (level-1)
                       | and [level==0, checkString]      = i
                       | otherwise                        = zoekString s t (i+1) level
                         where checkString = (until (checkChar s t i) (+1) 0) >= length t





vereenvoudig :: Prop -> Prop
vereenvoudig (En [p, En ps]) = En ([p] ++ ps)
vereenvoudig (Of [p, Of ps]) = Of([p] ++ ps)
vereenvoudig p = p


ontleed    :: String -> Prop
ontleed ps | i1 >= 0             = (linkerterm i1) :-> (rechterterm  i1)
            | i2 >= 0             = vereenvoudig (Of [linkerterm, rechterterm])
            | i3 >= 0             = vereenvoudig (En [linkerterm, rechterterm])
            | eqChar (head s) '!' = Niet (ontleed (tail s))
            | haakjes             = ontleed (init (tail s))
            | eqString s "T"      = Bool True
            | eqString s "F"      = Bool False
            | otherwise           = Var s
              where i1 = zoekString s "->" 0 0
                    i2 = zoekString s "\\/" 0 0
                    i3 = zoekString s "/\\" 0 0
                    s  = filter (notIsSpace) ps
                    notIsSpace ch = not (isSpace ch)
                    haakjes       = and [eqChar (head s) '(', eqChar (last s) ')']
                    linkerterm i  = ontleed (take i s)
                    rechterterm i = ontleed (drop (i+2) s)












evalueer'     :: String -> Bedeling -> Bool
evalueer'     s ps = evalueer(ontleed s) ps

vervulbaar'   :: String -> [Bedeling]
vervulbaar'   s = vervulbaar(ontleed s)

tautologie'   :: String -> Bool
tautologie'   s = tautologie(ontleed s)

contradictie' :: String -> Bool
contradictie' s = contradictie(ontleed s)

equivalent'   :: String -> String -> Bool
equivalent'   s t = equivalent (ontleed s) (ontleed t)

deMorgan'     :: String -> String
deMorgan'     s = toon(deMorgan(ontleed s))

-- missing parameters i2, i3 respectively
-- 197,55-64   197,67-77   197,54-78   198,55-64   198,67-77   198,54-78
