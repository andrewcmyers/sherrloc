module Propositie where

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
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqChar      :: Char -> Char -> Bool
eqChar = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

type Bedeling = [String]



evalueer :: Prop -> Bedeling -> Bool
evalueer (En x) y = and (map (`evalueer` y) x)
evalueer (Of x) y = or (map (`evalueer` y) x)
evalueer (Niet x) y = not (evalueer x y)
evalueer (x :-> q) y = evalueer p y
                         where p = Of [Niet x, q]
evalueer (Var x) y = elemBy eqString x y
evalueer (Bool x) _ = x


subs :: [a]->[[a]]
subs [] = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
              where subsxs = subs xs

varLijst :: Prop  -> Bedeling
varLijst (Bool _) = []
varLijst (Var x) = x:[]
varLijst (En []) = []
varLijst (Of []) = []
varLijst (En (x:xs)) = (varLijst x)++(varLijst (En xs))
varLijst (Of (x:xs)) = (varLijst x)++(varLijst (Of xs))
varLijst (Niet x) = varLijst x
varLijst (x :-> q) = (varLijst x)++(varLijst q)

vervulbaar :: Prop -> [Bedeling]
vervulbaar x = filter (evalueer x) (subs (varLijst x))


tautologie :: Prop -> Bool
tautologie x = eqList (eqList (eqString)) (vervulbaar x) (subs (varLijst x))


contradictie :: Prop -> Bool
contradictie x = eqList (eqList (eqString)) (vervulbaar x) ([])








checkPrioriteit :: Int -> Int -> String
checkPrioriteit y x | x<=y = ""
                    | otherwise = "("

checkPrioriteitEnd :: Int -> Int -> String
checkPrioriteitEnd y x | x<=y = ""
                       | otherwise = ")"

toonPrioritaire :: Prop -> Int -> String
toonPrioritaire (En []) _ = ""
toonPrioritaire (En (x:xs)) y | not(null xs) = (p++toonPrioritaire x 2++" /\\ "++toonPrioritaire (En xs) 2++q)
                              | otherwise = toonPrioritaire x 2
                              where p = checkPrioriteit y 2
                                    q = checkPrioriteitEnd y 2
toonPrioritaire (Of []) _ = ""
toonPrioritaire (Of (x:xs)) y | not(null xs) = (p++toonPrioritaire x 3++" \\/ "++toonPrioritaire (Of xs) 3++q)
                              | otherwise = toonPrioritaire x 3
                              where p = checkPrioriteit y 3
                                    q = checkPrioriteitEnd y 3
toonPrioritaire (Niet x) _ = ("!"++toonPrioritaire x 1)
toonPrioritaire (x :-> z) y = (p++toonPrioritaire x 4++" -> "++toonPrioritaire z 4++q)
                              where p = checkPrioriteit y 3
                                    q = checkPrioriteitEnd y 3
toonPrioritaire (Var x) _ = x
toonPrioritaire (Bool x) _ | x = " True "
                           | otherwise = " False "

toon :: Prop -> String
toon (En []) = ""
toon (En (x:xs)) | not(null xs) = (toonPrioritaire x 2++" /\\ "++toonPrioritaire (En xs) 2)
                 | otherwise = toonPrioritaire x 2
toon (Of []) = ""
toon (Of (x:xs)) | not(null xs) = (toonPrioritaire x 3++" \\/ "++toonPrioritaire (Of xs) 3)
                 | otherwise = toonPrioritaire x 3
toon (Niet x) = ("!"++toonPrioritaire x 1)
toon (x :-> z) = (toonPrioritaire x 4++" -> "++toonPrioritaire z 4)
toon (Var x) = x
toon (Bool x) | x = " True "
              | otherwise = " False "


equivalent  :: Prop -> Prop -> Bool
equivalent x y = eqList (eqBool) (map (evalueer x) q) (map (evalueer y) q)
                 where q = subs (varLijst x++varLijst y)


deMorgan :: Prop -> Prop
deMorgan (Niet (En x)) = Of  (map (Niet) (deMorgan x))
deMorgan (Niet (Of x)) = En  (map (Niet) (deMorgan x))
deMorgan (x :-> y) = deMorgan x :-> deMorgan y
deMorgan (Niet x) = Niet (deMorgan x)
deMorgan x = x


takeFrom :: (a -> Bool) -> [a] -> [a]
takeFrom _ [] = []
takeFrom  p (x:xs) | p x = takeFrom p xs
                   | otherwise = xs

propTokenizer :: String -> [String]
propTokenizer [] = []
propTokenizer xs = (takeWhile (not.eqChar ' ') xs):propTokenizer (takeFrom (not.eqChar ' ') xs)




sticky :: Prop -> Char -> Prop
sticky (Var x) y = Var (x++[y])
sticky x _ = x

ontleed2 :: String -> Bool -> Prop -> Prop
ontleed2 [] _ y = y
ontleed2 (x:xs) p y | eqString "/\\" (x:(head xs):[]) = En [y, (ontleed2 (tail (tail xs)) False y)]
                    | eqString "\\/" (x:(head xs):[]) = Of [y, (ontleed2 (tail (tail xs)) False y)]
                    | eqChar x '!' = Niet (ontleed2 xs False y)
                    | eqChar x '-' && eqChar (head xs) '>' = y :-> (ontleed2 (tail (tail xs)) False y)
                    | eqString "True" (x:take 3 xs)  = ontleed2 (tail (tail (tail xs))) True (Bool True)
                    | eqString "False" (x:take 4 xs) = ontleed2 (tail (tail (tail (tail xs)))) True (Bool False)
                    | eqChar ' ' x = ontleed2 xs True y
                    | not p = ontleed2 xs True (Var (x:[]))
                    | otherwise = ontleed2 xs True (sticky y x)

-- should be: map (...)
-- 116,43-52   116,43-50    117,43-52   117,42-50
