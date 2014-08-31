module Propositie where

data Prop
   = En [Prop]
   | Of [Prop]
   | Niet Prop
   | Prop :-> Prop
   | Var String
   | Bool Bool

eqString      :: String -> String -> Bool 
eqString = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqChar      :: Char -> Char -> Bool
eqChar = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined
elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined

type Bedeling                     = [String]







subs                              :: [a]->[[a]]
subs []                           = [[]]
subs (x:xs)                       = map (x:) (subs xs)
                                    ++(subs xs)









evalueer                          :: Prop -> Bedeling -> Bool
evalueer (En x) y                 = and (map (`evalueer` y) x)
evalueer (Of x) y                 = or (map (`evalueer` y) x)
evalueer (Niet x) y               = not (evalueer x y)
evalueer (x :-> q) y              = evalueer p y
   where p                        = Of [Niet x, q]
evalueer (Var x) y                = elemBy eqString x y
evalueer (Bool x) _               = x











varLijst                          :: Prop  -> Bedeling
varLijst (Bool _)                 = []
varLijst (Var x)                  = x:[]
varLijst (En [])                  = []
varLijst (Of [])                  = []
varLijst (En (x:xs))              = (varLijst x)
                                    ++(varLijst (En xs))
varLijst (Of (x:xs))              = (varLijst x)
                                    ++(varLijst (Of xs))
varLijst (Niet x)                 = varLijst x
varLijst (x :-> q)                = (varLijst x)
                                    ++(varLijst q)







vervulbaar                        :: Prop -> [Bedeling]
vervulbaar x                      = filter
                                    (evalueer x)
                                    (subs (varLijst x))












tautologie                        :: Prop -> Bool
tautologie x                      = eqList
                                    (eqList (eqString))
                                    (vervulbaar x)
                                    (subs (varLijst x))












contradictie                      :: Prop -> Bool
contradictie x                    = eqList
                                    (eqList (eqString))
                                    (vervulbaar x)
                                    ([])


























checkPrioriteit                   :: Int -> Int -> String
checkPrioriteit x y
   | x>y                         = ""
   | otherwise                    = "("


checkPrioriteitEnd                :: Int -> Int -> String
checkPrioriteitEnd x y
   | x>y                         = ""
   | otherwise                    = ")"






toonPrio                          :: Prop -> Int -> String
toonPrio (En []) _                = ""
toonPrio (En (x:xs)) y
   | not(null xs)                 = (checkPrioriteit y 2
                                    ++toonPrio x 2
                                    ++" /\\ "
                                    ++toonPrio (En xs) 2
                                    ++checkPrioriteitEnd y 2)
   | otherwise                    = toonPrio x 2
toonPrio (Of []) _                = ""
toonPrio (Of (x:xs)) y
   | not(null xs)                 = (checkPrioriteit y 3
                                    ++toonPrio x 3
                                    ++" \\/ "
                                    ++toonPrio (Of xs) 3
                                    ++checkPrioriteitEnd y 3)
   | otherwise                    = toonPrio x 3
toonPrio (Niet x) _               = ("!"
                                    ++toonPrio x 1)
toonPrio (x :-> z) y              = (checkPrioriteit y 4
                                    ++toonPrio x 4
                                    ++" -> "
                                    ++toonPrio z 4
                                    ++checkPrioriteitEnd y 4)
toonPrio (Var x) _                = x
toonPrio (Bool x) _
   | x                            = " True "
   | otherwise                    = " False "



toon                              :: Prop -> String
toon (En [])                      = ""
toon (En (x:xs))
   | not(null xs)                 = (toonPrio x 2
                                    ++" /\\ "
                                    ++toonPrio (En xs) 2)
   | otherwise                    = toonPrio x 2
toon (Of [])                      = ""
toon (Of (x:xs))
   | not(null xs)                 = (toonPrio x 3
                                    ++" \\/ "
                                    ++toonPrio (Of xs) 3)
   | otherwise                    = toonPrio x 3
toon (Niet x)                     = ("!"
                                    ++toonPrio x 1)
toon (x :-> z)                    = (toonPrio x 4
                                    ++" -> "
                                    ++toonPrio z 4)
toon (Var x)                      = x
toon (Bool x)
   | x                            = " True "
   | otherwise                    = " False "
















equivalent                        :: Prop -> Prop -> Bool
equivalent x y                    = eqList
                                    (eqBool)
                                    (map (evalueer x) q)
                                    (map (evalueer y) q)
    where q                       = subs
                                    (varLijst x++varLijst y)
















deMorgan                          :: Prop -> Prop
deMorgan (Niet (En x))            = Of
                                    (map (deMorgan)
                                    (map (Niet) x))
deMorgan (Niet (Of x))            = En
                                    (map (deMorgan)
                                    (map (Niet) x))
deMorgan (x :-> y)                = deMorgan x :-> deMorgan y
deMorgan (Niet x)                 = Niet (deMorgan x)
deMorgan x                        = x






seperateElem                      :: String -> Int -> String -> [String]
seperateElem [] 0 z               = (init z):[[init z]]
seperateElem [] _ _               = [[],[]]
seperateElem x 0 z                = (init z):[(init z++x)]
seperateElem (x:xs) y z
   | eqChar x '('                 = seperateElem xs (y+1) (z++[x])
   | eqChar x ')'                 = seperateElem xs (y-1) (z++[x])
   | otherwise                    = seperateElem xs (y) (z++[x])

ontleed2                                  :: String -> String -> String
ontleed2 [] y                             = y
ontleed2 (x:xs) y
   | eqString "/\\" (x:(head xs):[])      = "En ["++y++", "++(ontleed2(tail (tail xs)) [])++"]"
   | eqString "\\/" (x:(head xs):[])      = "Of ["++y++", "++(ontleed2(tail (tail xs)) [])++"]"
   | eqChar x '!'                         = "Niet ("++(ontleed2 xs y)++")"
   | eqChar x '-' && eqChar (head xs) '>' = y ++ " :-> "++(ontleed2(tail (tail xs)) [])
   | eqString "True" (x:take 3 xs)        = "Bool True "++(ontleed2 (tail(tail (tail xs))) [])
   | eqString "False" (x:take 4 xs)       = "Bool False "++(ontleed2(tail (tail (tail (tail xs)))) [])
   | eqChar x '('                         = "("++ontleed2 (head q) y ++")"++ontleed2 (last q) y
   | eqChar ' ' x                         = ontleed2 xs y
   | null y                               = ontleed2 xs ("Var "++[x])
   | otherwise                            = ontleed2 xs (y++[x])
   where q                                = seperateElem xs 1 []

-- [[init z]] should be [init z]
-- 270,48-53   270,47-54   270,46-55
