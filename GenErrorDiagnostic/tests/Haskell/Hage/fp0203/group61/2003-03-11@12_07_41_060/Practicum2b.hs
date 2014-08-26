module Practicum2b where

infixr :->

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
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined
eqChar      :: Char -> Char -> Bool 
eqChar = undefined
isAlpha :: Char -> Bool
isAlpha = undefined
isSpace :: Char -> Bool
isSpace = undefined
elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined

evalueer :: Prop -> Bedeling -> Bool
evalueer (Bool a) _                 = a
evalueer (Var x) bedeling           = elemBy eqString x bedeling
evalueer (Niet a) bedeling          = not (evalueer a bedeling)
evalueer (En []) _                  = True
evalueer (En (prop:props)) bedeling = evalueer prop bedeling && evalueer (En props) bedeling
evalueer (Of []) _                  = False
evalueer (Of (prop:props)) bedeling = evalueer prop bedeling || evalueer (Of props) bedeling
evalueer (a :-> b) bedeling         = evalueer (Niet a) bedeling || evalueer b bedeling


vervulbaar :: Prop -> [Bedeling]
vervulbaar propositie = filter (evalueer propositie) (subs (plukVariabelen propositie))


plukVariabelen :: Prop -> [String]
plukVariabelen (Var a)           = [a]
plukVariabelen (Bool _)          = []
plukVariabelen (Niet _)          = []
plukVariabelen (En [])           = []
plukVariabelen (En (prop:props)) = plukVariabelen prop ++ plukVariabelen (En props)
plukVariabelen (Of [])           = []
plukVariabelen (Of (prop:props)) = plukVariabelen prop ++ plukVariabelen (Of props)
plukVariabelen (prop1 :-> prop2) = plukVariabelen prop1 ++ plukVariabelen prop2


subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
              where subsxs = subs xs


tautologie :: Prop -> Bool
tautologie propositie = foldl1 (&&) (map (evalueer propositie) (subs (plukVariabelen propositie)))


contradictie :: Prop -> Bool
contradictie propositie | null (vervulbaar propositie) = True
                        | otherwise = False



toon :: Prop -> String
toon (Var a)      = a
toon (Bool True)  = "T"
toon (Bool False) = "F"


toon (En ((Of props):[]))            = "(" ++ toon (Of props) ++ ")"
toon (En ((prop1:->prop2):[]))       = "(" ++ toon (prop1:->prop2) ++ ")"
toon (En (prop:[]))                  = toon prop
toon (En ((Of propsOf):props))       = "(" ++ toon (Of propsOf) ++") /\\ " ++ toon (En props)
toon (En ((prop1:->prop2):props))    = "(" ++ toon (prop1:->prop2) ++") /\\ " ++ toon (En props)
toon (En (prop:props))               = toon prop ++ " /\\ " ++ toon (En props)


toon (Of ((prop1:->prop2):[]))       = "(" ++ toon (prop1:->prop2) ++ ")"
toon (Of (prop:[]))                  = toon prop
toon (Of ((prop1:->prop2):props))    = "(" ++ toon (prop1:->prop2) ++ ") \\/ " ++ toon (Of props)
toon (Of (prop:props))               = toon prop ++ " \\/ " ++ toon (Of props)


toon ((prop1a :-> prop1b) :-> prop2) = "(" ++ toon (prop1a :-> prop1b) ++ ") -> " ++ toon prop2
toon (prop1 :-> prop2)               = toon prop1 ++ " -> " ++ toon prop2


toon (Niet (Bool a))                 = "!" ++ toon (Bool a)
toon (Niet (Var a))                  = "!" ++ toon (Var a)
toon (Niet prop)                     = "!(" ++ toon prop ++ ")"


equivalent :: Prop -> Prop -> Bool
equivalent prop1 prop2 = foldr1 (&&) (map (\x->eqBool (evalueer prop1 x) (evalueer prop2 x)) (subs (plukVariabelen prop1 ++ plukVariabelen prop2)))


deMorgan :: Prop -> Prop
deMorgan (Niet(Niet prop)) = deMorgan prop
deMorgan (Niet(En props))  = Of (map (deMorgan.Niet) props)
deMorgan (Niet(Of props))  = En (map (deMorgan.Niet) props)
deMorgan (prop1 :-> prop2) = deMorgan prop1 :-> deMorgan prop2
deMorgan (Niet prop)       = Niet (deMorgan prop)
deMorgan (En props)        = En (map (deMorgan) props)
deMorgan (Of props)        = Of (map (deMorgan) props)
deMorgan prop              = prop


vindSplits :: String -> Int -> Maybe Int -> Int -> Int
vindSplits [] positie _ _ = positie
vindSplits geheel@(char:chars) positie hoogte niveau
   | eqChar char '('
      = vindSplits chars positie hoogte (niveau+10)
   | eqChar char ')'
      = vindSplits chars positie hoogte (niveau-10)
   | eqChar char '!' && (niveau +4) `kleiner` hoogte
      = vindSplits chars (length geheel) (Just (niveau + 4)) niveau
   | eqString (take 2 geheel) "/\\" && (niveau + 3) `kleiner` hoogte
      = vindSplits (drop 2 geheel) (length geheel) (Just (niveau + 3)) niveau
   | eqString (take 2 geheel) "\\/" && (niveau + 2) `kleiner` hoogte
      = vindSplits (drop 2 geheel) (length geheel) (Just (niveau + 2)) niveau
   | eqString (take 2 geheel) "->" && (niveau + 1) `kleiner` hoogte
      = vindSplits (drop 2 geheel) (length geheel) (Just (niveau + 1)) niveau
   | otherwise
      = vindSplits (dropWhile isAlpha chars) positie hoogte niveau


kleiner :: Int -> Maybe Int -> Bool
kleiner x (Just y) = x<y
kleiner _ Nothing  = True


ontleed :: String -> Prop
ontleed tekst = maakSimpel (parse (filter (not.isSpace) tekst))



parse :: String -> Prop
parse tekst
   | eqString (take 2 (drop splits tekst)) "\\/"
      = Of [parse (take (splits) tekst), parse (drop (splits+2) tekst)]
   | eqString (take 2 (drop splits tekst)) "/\\"
      = En [parse (take (splits) tekst), parse (drop (splits+2) tekst)]
   | eqString (take 1 (drop splits tekst)) "!"
      = Niet (parse (drop (splits+1) tekst))
   | eqString (take 2 (drop splits tekst)) "->"
      = parse (take (splits) tekst) :-> parse (drop (splits+2) tekst)
   | eqString (take 1 tekst) "(" && eqString (drop ((length tekst) - 1) tekst) ")"
      = parse (tail (take ((length tekst) - 1) tekst))
   | eqString rest "T"
      = Bool True
   | eqString rest "F"
      = Bool False
   | otherwise
      = Var rest
   where splits = length tekst - vindSplits tekst 0 Nothing 0
         rest   = dropWhile (eqChar '(') (takeWhile (not.eqChar ')')tekst)
















maakSimpel :: Prop -> Prop
maakSimpel (En props) = En (concatMap vindEnLijst props)
maakSimpel (Of props) = Of (concatMap vindOfLijst props)
maakSimpel prop = prop

vindEnLijst :: Prop -> [Prop]
vindEnLijst (En props) = concatMap vindEnLijst (maakSimpel props)
vindEnLijst prop = [prop]

vindOfLijst :: Prop -> [Prop]
vindOfLijst (Of props) = concatMap vindOfLijst (maakSimpel props)
vindOfLijst prop = [prop]

-- should be: map (...)
-- 185,49-64   189,49-64
