module Logica where

infixr 4 :->
infixr 6  <*>
infixl 5  <@
infixr 4  <|>

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
isAlpha :: Char -> Bool
isAlpha = undefined

type Bedeling = [String]
type Parser a = String -> [( a, String )]


evalueer :: Prop -> Bedeling -> Bool
evalueer (En ps)     bed = and $ map (flip evalueer bed) ps
evalueer (Of ps)     bed = or  $ map (flip evalueer bed) ps
evalueer (Niet p)    bed = not $ evalueer p bed
evalueer (p1 :-> p2) bed = evalueer (Niet p1) bed || evalueer p2 bed
evalueer (Var s)     bed = elemBy eqString s bed
evalueer (Bool b)    _   = b



vervulbaar :: Prop -> [Bedeling]
vervulbaar p = filter (evalueer p) (segs $ uniek $ filterVar p)

filterVar :: Prop -> [String]
filterVar (En ps)     = concat $ map filterVar ps
filterVar (Of ps)     = concat $ map filterVar ps
filterVar (Niet p)    = filterVar p
filterVar (p1 :-> p2) = filterVar p1 ++ filterVar p2
filterVar (Var s)     = [s]
filterVar (Bool _)    = []

uniek :: [String] -> [String]
uniek []     = []
uniek (x:xs) | elemBy eqString x xs = uniek xs
             | otherwise            = x : (uniek xs)

segs :: [a] -> [[a]]
segs []     = [[]]
segs (x:xs) = map (x:) (inits xs) ++ segs xs

inits :: [a] -> [[a]]
inits []     = [[]]
inits (x:xs) = [] : map (x:) (inits xs)



tautologie :: Prop -> Bool
tautologie p = length (vervulbaar p) == length (segs $ filterVar p)



contradictie :: Prop -> Bool
contradictie p = length (vervulbaar p) == 0



toon :: Prop -> String
toon (Niet p)     = "!" ++ toon' p 1
toon (En [])      = ""
toon (En [p])     = toon' p 2
toon (En (p:ps))  = toon' p 2 ++ " /\\ " ++ toon (En ps)
toon (Of [])      = ""
toon (Of [p])     = toon' p 3
toon (Of (p:ps))  = toon' p 3 ++ " \\/ " ++ toon (Of ps)
toon (p1 :-> p2)  = toon' p1 3 ++ " -> " ++ toon' p2 4
toon (Var s)      = s
toon (Bool b)     | b `eqBool` True = "True"
                  | otherwise       = "False"

toon' :: Prop -> Int -> String
toon' (En ps)     x = zetHaken x 2 (toon (En ps))
toon' (Of ps)     x = zetHaken x 3 (toon (Of ps))
toon' (p1 :-> p2) x = zetHaken x 4 (toon (p1 :-> p2))
toon' p           _ = toon p

zetHaken :: Int -> Int -> String -> String
zetHaken x y str | x<y       = "(" ++ str ++ ")"
                 | otherwise = str



equivalent :: Prop -> Prop -> Bool
equivalent p1 p2 = tautologie (En [(p1 :-> p2),(p2 :-> p1)] )



deMorgan :: Prop -> Prop
deMorgan (Niet (En ps))     = Of (map deMorgan (map Niet ps))
deMorgan (Niet (Of ps))     = En (map deMorgan (map Niet ps))
deMorgan (Niet (Niet p))    = deMorgan p
deMorgan (En ps)            = En (map deMorgan ps)
deMorgan (Of ps)            = Of (map deMorgan ps)
deMorgan (Niet p)           = Niet $ deMorgan p
deMorgan (p1 :-> p2)        = deMorgan p1 :-> deMorgan p2
deMorgan (Var s)            = Var s
deMorgan (Bool b)           = Bool b




letter :: Char -> Parser Char
letter _ [] = []
letter a (x:xs) | x `eqChar` a = [ (a, xs) ]
                | otherwise = []

tok :: String -> Parser String
tok k xs | k `eqString` take n xs = [ (k, drop n xs) ]
         | otherwise = []
         where n = length k

(<*>)          ::  Parser a -> Parser b -> Parser (a, b)
(p1 <*> p2) xs  =  [  ((v1,v2),xs2)
                   |  (v1,xs1) <- p1 xs
                   ,  (v2,xs2) <- p2 xs1
                   ]

(<|>)          ::  Parser a -> Parser a -> Parser a
(p1 <|> p2) xs  =  p1 xs ++ p2 xs

just :: Parser a -> Parser a
just p xs = [ (v,"") | (v,ys) <- p xs, ys `eqString` ""]

(<@)           ::  Parser a -> (a -> b) -> Parser b
(p <@ f) xs     =  [ (ys, f v)
                   | (ys,   v) <- p xs
                   ]







variabele :: Parser String
variabele xs = [ (takeWhile isAlpha xs, dropWhile isAlpha xs) ]

_prop :: Parser Prop
_prop = _of
       <|> ((_of <*> (tok "->") <*> _prop) <@ (\(e1,(_,e2)) -> e1 :-> e2))

_of :: Parser Prop
_of = _en
       <|> ((_en <*> (tok "\\/") <*> _of) <@ (\(e1,(_,e2)) -> Of [e1,e2]))

_en :: Parser Prop
_en = _niet
       <|> ((_niet <*> (tok "/\\") <*> _en) <@ (\(e1,(_,e2)) -> En [e1,e2]))

_niet :: Parser Prop
_niet = _atoom
       <|> (((letter '!') <*> _niet) <@ (\(_,e) -> Niet e))

_atoom :: Parser Prop
_atoom = ((tok "True")                                    <@ (\_ -> (Bool True)))
         <|> ((tok "False")                               <@ (\_ -> (Bool False)))
         <|> (variabele                                   <@ Var)
         <|> ((letter '(' <*> _prop <*> letter ')') <@ (\(_,(e,_)) -> e))

ontleed :: String -> Prop
ontleed str = fst $ head $ just _prop (filter (not . eqChar ' ') str)

-- should swap (f v) and ys
-- 146,23-24   146,27-29
