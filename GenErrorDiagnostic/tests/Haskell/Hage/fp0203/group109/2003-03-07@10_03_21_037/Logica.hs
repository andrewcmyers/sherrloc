module Logica where
elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
showBool       :: Bool -> String
showBool = undefined
ordString :: String -> String -> Ordering
ordString = undefined
test1::Prop
test2::Prop
test3::Prop
test4::Prop
test5::Prop
test6::Prop
test7::Prop
test8::Prop
test1 = En [Var "p", Var "q"]
test2 = (Of [En [Var "p",Var "q"], Var "r"])
test3 = Of [Var "p", Niet (Var "p")]
test4 = En [Var "p", Niet (Var "p")]
test5 = (En [Of [Var "p",Var "q"], Var "r"])

test6 = Of [Niet (En [Var "a", Bool True]), Niet (Var "f")]

test7 = Of [Var "v", Var "w"]
test8 = Niet (En [Niet (Var "w"),Niet( Var "v")])



data Prop
   = En [Prop]
   | Of [Prop]
   | Niet Prop
   | Prop :-> Prop
   | Var String
   | Bool Bool

type Bedeling = [String]



evalueer :: Prop -> Bedeling -> Bool
vervulbaar :: Prop -> [Bedeling]
tautologie :: Prop -> Bool
contradictie :: Prop -> Bool
toon :: Prop -> String
equivalentie :: Prop -> Prop -> Bool
deMorgan :: Prop -> Prop


removeDuplicates :: [String] -> [String]
getVariables :: Prop -> [String]
subs :: [a] -> [[a]]
alleMogelijkheden :: Prop -> [Bedeling]
voegIn :: [a] -> [[a]] -> [a]
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
morganTest :: Prop -> Bool
vereenvoudig :: Prop -> Prop
zetOm :: Int -> Prop ->String
metHaakjes :: String -> String



evalueer (En props)    bedel = and (map (flip evalueer bedel) props)
evalueer (Of props)    bedel = or  (map (\x -> evalueer x bedel) props)
evalueer (Niet prop)   bedel = not (evalueer prop bedel)
evalueer (hyp :-> con) bedel | evalueer hyp bedel = evalueer con bedel
                             | otherwise          = True
evalueer (Var v)       bedel = elemBy eqString v bedel
evalueer (Bool b)      _     = b

vervulbaar prop = filter (evalueer prop) (alleMogelijkheden prop)

tautologie prop = length (vervulbaar prop) == length (alleMogelijkheden prop)

contradictie prop = null (vervulbaar prop)

















equivalentie p1 p2 = eqList (eqList eqString) (vervulbaar p1) (vervulbaar p2)

deMorgan prop | morganTest prop = deMorgan $ vereenvoudig prop
              | otherwise       = prop


removeDuplicates []     = []
removeDuplicates (z:zs) | elemBy eqString z zs = removeDuplicates zs
                        | otherwise            = z : removeDuplicates zs

getVariables (En props)    = concatMap getVariables props
getVariables (Of props)    = concatMap getVariables props
getVariables (Niet prop)   = getVariables prop
getVariables (hyp :-> con) = getVariables hyp ++ getVariables con
getVariables (Var a)       = [a]
getVariables  _            = []

subs []     = [[]]
subs (x:xs) = map (x:) (subs xs) ++ subs xs

alleMogelijkheden = subs . removeDuplicates . (sortBy ordString) . getVariables

voegIn symbol list = foldr1 (\ x y -> x ++ symbol ++ y) list

sortBy cmp       = foldr (insertBy cmp) []

insertBy _   x []    = [x]
insertBy cmp x ys@(y:ys')
             = case cmp x y of
                GT -> y : insertBy cmp x ys'
                _  -> x : ys

morganTest (Niet (En _)) = True
morganTest (Niet (Of _)) = True
morganTest (Niet prop)   = morganTest prop
morganTest (En props)    = or $ map morganTest props
morganTest (Of props)    = or $ map morganTest props
morganTest (hyp :-> con) = morganTest hyp || morganTest con
morganTest (Var _)       = False
morganTest (Bool _)      = False

vereenvoudig (Niet (En props)) = Of (map ( \p -> vereenvoudig (Niet p)) props)
vereenvoudig (Niet (Of props)) = En (map ( \p -> vereenvoudig (Niet p)) props)
vereenvoudig (Niet prop)       = Niet (vereenvoudig prop)
vereenvoudig (En props)        = En (map vereenvoudig props)
vereenvoudig (Of props)        = Of (map vereenvoudig props)
vereenvoudig (hyp :-> con)     = vereenvoudig hyp :-> vereenvoudig con
vereenvoudig prop              = prop

toon = zetOm 0

zetOm n (Niet prop) | n>4       = metHaakjes (zetOm 0 (Niet prop))
                    | otherwise = "!" ++ zetOm 4 prop
zetOm n (En props) | n>3       = metHaakjes (zetOm 0 (En props))
                   | otherwise = voegIn " /\\ " (map (zetOm 3) props)
zetOm n (Of props) | n>2       = metHaakjes (zetOm 0 (Of props))
                   | otherwise = voegIn " \\/ " (map (zetOm 3) props)
zetOm n (hyp :-> con) | n>=1      = metHaakjes (zetOm 0 (hyp :-> con))
                      | otherwise = zetOm 1.5 hyp ++ " -> " ++ zetOm 1.5 con
zetOm _ (Var a) = a
zetOm _ (Bool b) = showBool b

metHaakjes s = "(" ++ s ++ ")"

-- 1.5 should be 2 
-- 156,43-45   156,70-72
