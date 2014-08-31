module PropExt2 where
import Data.List

infixr 5 :->

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined
showBool       :: Bool -> String
showBool = undefined

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

type Bedeling = [String]

evalueer :: Prop -> Bedeling -> Bool
evalueer (Var s) p = elemBy eqString s p
evalueer (En s) p = and (map (flip evalueer p) s)
evalueer (Of s) p = or (map (flip evalueer p) s)
evalueer (Niet s) p = not (evalueer s p)
evalueer (a :-> b) p = evalueer (Of [Niet a,b]) p
evalueer (Bool x) _ = x

vervulbaar :: Prop -> [Bedeling]
vervulbaar a = filter (evalueer a) (subs (nubBy eqString (maakVarLijst a)))



maakVarLijst :: Prop -> Bedeling
maakVarLijst (Var s) = [s]
maakVarLijst (En s) = concatMap maakVarLijst s
maakVarLijst (Of s) = concatMap maakVarLijst s
maakVarLijst (Niet s) = maakVarLijst s
maakVarLijst (a :-> b) = maakVarLijst (Of [Niet a,b])
maakVarLijst (Bool _) = []

subs :: Bedeling -> [Bedeling]
subs [] = [ [] ]
subs (x:xs) = map (x:) (subs xs) ++ subs xs

tautologie :: Prop -> Bool
tautologie a = null (filter (not.(evalueer a)) (subs (maakVarLijst a)))

contradictie :: Prop -> Bool
contradictie a = null (vervulbaar a)
















toon :: Prop -> String
toon a = toon2 a 5

toon2 :: Prop -> Int -> String
toon2 (Var s) _ = s
toon2 (En s) a | a < 2 ="(" ++ (maakEnOf " /\\ " s 2) ++ ")"
               | otherwise = maakEnOf " /\\ " s 2
toon2 (Of s) a | a < 3 = "(" ++ (maakEnOf " \\/ " s 3) ++ ")"
               | otherwise = maakEnOf " \\/ " s 3
toon2 (Niet s) _ = "!" ++ (toon2 s 1)
toon2 (a :-> b) x | x<4 = "(" ++ ((toon2 a 3) ++ " -> " ++ (toon2 b 4)) ++ ")"
                  | otherwise = (toon2 a 3) ++ " -> " ++ (toon2 b 4)
toon2 (Bool a) _ = showBool a


maakEnOf :: String -> [Prop] -> Int -> String
maakEnOf _ [] _ = []
maakEnOf _ (x:[]) b = toon2 x b
maakEnOf a (x:xs) b = (toon2 x b) ++ a ++ (maakEnOf a xs b)

equivalent :: Prop -> Prop -> Bool
equivalent a b = and (map (test a b) (subs (nubBy eqString (maakVarLijst (En [a,b])))))


test :: Prop -> Prop -> Bedeling -> Bool
test a b x = eqBool (evalueer a x) (evalueer b x)

deMorgan :: Prop -> Prop
deMorgan (Niet (En a)) = deMorgan (Of (map Niet a))
deMorgan (Niet (Of a)) = deMorgan (En (map Niet a))
deMorgan (En a) = (En (map deMorgan a))
deMorgan (Of a) = (Of (map deMorgan a))
deMorgan (Niet a) = (Niet (deMorgan a))
deMorgan (a  :-> b) = ((deMorgan a) :-> (deMorgan b))
deMorgan a = a

ontleed :: String -> [String]
ontleed [] = []
ontleed lijst@('/':xs) = [(take 2 lijst)] ++ (ontleed (drop 1 xs))
ontleed lijst@('\\':xs) = [(take 2 lijst)] ++ (ontleed (drop 1 xs))
ontleed lijst@('-':xs) = [(take 2 lijst)] ++ (ontleed (drop 1 xs))
ontleed lijst@('!':xs) = [(take 1 lijst)] ++ (ontleed xs)
ontleed lijst@('(':xs) = [(take 1 lijst)] ++ (ontleed xs)
ontleed lijst@(')':xs) = [(take 1 lijst)] ++ (ontleed xs)
ontleed (x:xs) = [x] : (ontleed xs)

filterProp :: [String] -> [String]
filterProp a = filter (not.(eqString " ")) a

pakVar :: [String] -> [String]
pakVar [] = []
pakVar lijst@(x:xs)  | elemBy eqString  x ["/\\","->","\\/","!","(",")"] =  x : (pakVar xs)
                     | otherwise = concat(takeWhile (not.verG) lijst) : (pakVar (dropWhile (not.verG) lijst))

verG :: String -> Bool
verG a  = elemBy eqString a ["/\\","->","\\/","!","(",")"]

maakProp :: [String] -> Prop
maakProp (x:[]) | eqString x "T" = Bool True
                | eqString x "F" = Bool False
                | otherwise = Var x

maakProp (x:(y:xs)) | eqString x "(" = maakProp (y:xs)
                    | eqString x ")" = Var ")"
                    | eqString x "!" = Niet (maakProp (y:xs))
                    | eqString x "/\\" = (maakProp xs)
                    | eqString x "\\/" = (maakProp xs)
                    | eqString y "/\\" = En
                    | eqString y "\\/" = Of
                    | eqString y "->" =  (maakProp [x]) :-> (maakProp xs)

werkUit :: [String] -> [(Prop,String)]
werkUit (x:xs) | eqString x "(" = (werkUit (takeWhile (not.(eqString ")")) xs)) ++ (werkUit (dropWhile (not.(eqString ")")) xs))
               | otherwise = [(Var "0","einde")]

-- En should be: Var "En"; Of should be: Var "Of"
-- 137,42-43   138,42-43
