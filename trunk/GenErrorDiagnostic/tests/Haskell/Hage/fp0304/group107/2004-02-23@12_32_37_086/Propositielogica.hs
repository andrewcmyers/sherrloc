module Propositielogica where

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
nubBy        :: (a -> a -> Bool) -> [a] -> [a]
nubBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqChar      :: Char -> Char -> Bool
eqChar = undefined

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

type Bedeling = [String]




evalueer :: Prop -> Bedeling -> Bool
evalueer (Of x)      y = or (map (flip evalueer y) x)
evalueer (En x)      y = and (map (flip evalueer y) x)
evalueer (Bool x)    _ = x
evalueer (Niet x)    y = not (evalueer x y)
evalueer (xs :-> ys) y = (not (evalueer xs y)) || evalueer ys y
evalueer (Var x)     y = elemBy eqString x y




vervulbaar :: Prop -> [Bedeling]
vervulbaar x = filter (evalueer x) (subs (nubBy eqString (zoekVars x)))

zoekVars :: Prop -> [String]
zoekVars (Of x)      = concat (map zoekVars x)
zoekVars (En x)      = concat (map zoekVars x)
zoekVars (Bool _)    = []
zoekVars (Niet x)    = zoekVars x
zoekVars (xs :-> ys) = zoekVars xs ++ zoekVars ys
zoekVars (Var x)     = [x]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
            where subsxs = subs xs




tautologie :: Prop -> Bool
tautologie x = and (map (evalueer x) (subs (nubBy eqString (zoekVars x))))




contradictie :: Prop -> Bool
contradictie x = not ( or (map (evalueer x) (subs (nubBy eqString (zoekVars x)))))




toon :: Prop -> String




toon prop = lelijkeToon 5 prop

lelijkeToon :: Int -> Prop -> String
lelijkeToon prio (Of x)    | prio<3  = "(" ++ drop 4 (concatMap insertOf (map (lelijkeToon 3) x)) ++ ")"
                           | otherwise = drop 4 (concatMap insertOf (map (lelijkeToon 3) x))
                           where insertOf  = (++) " \\/ "
lelijkeToon prio (En x)    | prio<2  = "(" ++ drop 4 (concatMap insertOf (map (lelijkeToon 2) x)) ++ ")"
                           | otherwise = drop 4 (concatMap insertOf (map (lelijkeToon 2) x))
                           where insertOf  = (++) " /\\ "
lelijkeToon _    (Niet x)  = "!" ++ lelijkeToon 1 x
lelijkeToon _    (Bool x)  | x         = "True"
                           | otherwise = "False"
lelijkeToon prio (x :-> y) | prio<4 = "(" ++ lelijkeToon 4 x ++ " -> " ++ lelijkeToon 4 y ++ ")"
                           | otherwise = lelijkeToon 4 x ++ " -> " ++ lelijkeToon 4 y
lelijkeToon _    (Var x)   = x
















equivalent :: Prop -> Prop -> Bool
equivalent x y = and (map (evalueer x) (subs (nubBy eqString (zoekVars x ++ zoekVars y))))




deMorgan :: Prop -> Prop
deMorgan (Niet (En x)) = Of (map (deMorgan . Niet) x)
deMorgan (Niet (Of x)) = En (map (deMorgan . Niet) x)
deMorgan (En x)    = En (map deMorgan x)
deMorgan (Of x)    = Of (map deMorgan x)
deMorgan (Niet x)  = Niet (deMorgan x)
deMorgan (Bool x)  = Bool x
deMorgan (Var x)   = Var x
deMorgan (x :-> y) = (deMorgan x) :-> (deMorgan y)



ontleed :: String -> Prop
ontleed proph | low `eqString` " /\\ " = En (map ontleed (ontleedBy " /\\ " ([],[],prop) 0))
             | low `eqString` " \\/ " = Of (map ontleed (ontleedBy " \\/ " ([],[],prop) 0))

             | low `eqString` "!"     = Niet (ontleed (drop 1 prop))
             | low `eqString` "B"     = Bool (getBool prop)
             | otherwise              = Var prop
             where (x:xs) = ontleedBy " -> " ([],[],prop) 0
                   low = getLowest prop 0 (-10)
                   prop = wegHaakjes proph 0

ontleedBy :: String -> ([String],String,String) -> Int -> [String]
ontleedBy _ (eindLijst,tussenResultaat,[]) _ = reverse (tussenResultaat:eindLijst)
ontleedBy str (eindLijst,tussenResultaat,(x:xs)) h  | x `eqChar` '(' = ontleedBy str (eindLijst,tussenResultaat++[x],xs) (h+1)
                                                             | x `eqChar` ')' = ontleedBy str (eindLijst,tussenResultaat++[x],xs) (h-1)
                                                             | h==0 && take (length str) (x:xs) `eqString` str = ontleedBy str (tussenResultaat:eindLijst,[],drop (length str) (x:xs)) h
                                                             | otherwise = ontleedBy str (eindLijst,tussenResultaat++[x],xs) h

getLowest :: String -> Int -> Int -> String
getLowest [] _ p | p==1 = "!"
                 | p==2 = " /\\ "
                 | p==3 = " \\/ "
                 | p==4 = " -> "
                 | p==0 = "B"
                 | otherwise = "V"
getLowest lijst@(x:xs) h p | x `eqChar` '(' = getLowest xs (h+1) p
                           | x `eqChar` ')' = getLowest xs (h-1) p
                           | take 4 lijst `eqString` " /\\ " && h==0 && p<2 = getLowest (drop 4 lijst) h 2
                           | take 4 lijst `eqString` " \\/ " && h==0 && p<3 = getLowest (drop 4 lijst) h 3
                           | take 4 lijst `eqString` " -> " && h==0 && p<4 = getLowest (drop 4 lijst) h 4
                           | take 1 lijst `eqString` "!" && h==0 && p<1 = getLowest (drop 1 lijst) h 1
                           | take 4 lijst `eqString` "True" && h==0 && p<0 = getLowest (drop 4 lijst) h 0
                           | take 5 lijst `eqString` "False" && h==0 && p<0 = getLowest (drop 5 lijst) h 0
                           | otherwise = getLowest xs h p

getBool :: String -> Bool
getBool [] = False
getBool str | str `eqString` "True" = True
            | otherwise = False

wegHaakjes :: (String,String) -> Int -> String
wegHaakjes (_,[]) _ = []
wegHaakjes (result,(x:xs)) h | not (eqChar x '(') && null result  = x:xs
                             | h==1 && null xs = drop 1 result
                             | eqChar x '('  = wegHaakjes (result++[x],xs) (h+1)
                             | eqChar x ')' = wegHaakjes (result++[x],xs) (h-1)
                             | h==0 && not(null xs) = result++(x:xs)
                             | otherwise = wegHaakjes (result++[x],xs) h

-- proph should be: ([],proph)
-- 130,38-42
