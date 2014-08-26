module Proposlo where
data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

type Bedeling = [String]

testBed :: Bedeling
testBed = ["p","q"]
elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

evalueer :: Prop -> Bedeling -> Bool
evalueer (Bool prop) _ = prop
evalueer (Var letter) bedeling | elemBy eqString letter bedeling = True
                               | otherwise = False
evalueer (Niet prop) bedeling = not (evalueer prop bedeling)
evalueer (En []) _ = True
evalueer (En (prop1:props)) bedeling = evalueer prop1 bedeling && evalueer (En props) bedeling
evalueer (Of []) _ = False
evalueer (Of (prop1:props)) bedeling = evalueer prop1 bedeling || evalueer (Of props) bedeling
evalueer (prop1 :-> prop2) bedeling = evalueer (Of [Niet prop1, prop2]) bedeling



letters :: Prop -> Bedeling
letters (Bool _) = []
letters (Var letter) = [letter]
letters (Niet prop) = letters prop
letters (En []) = []
letters (En (prop1:props)) = letters prop1 ++ letters (En props)
letters (Of []) = []
letters (Of (prop1:props)) = letters prop1 ++ letters (Of props)
letters (prop1 :-> prop2) = letters prop1 ++ letters prop2

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
              where subsxs = subs xs

vervulbaar :: Prop -> [Bedeling]
vervulbaar prop = filter (evalueer prop) (subs(letters prop))



tautologie :: Prop -> Bool
tautologie prop = all (evalueer prop) (subs(letters prop))




contradictie :: Prop -> Bool
contradictie prop = all (not.evalueer prop) (subs(letters prop))




toon :: Prop -> String
toon prop = (toonProp prop) ++ "\n"














































toonProp :: Prop -> String
toonProp (Bool bool) | bool = "True"
                     | otherwise = "False"
toonProp (Var letter) = letter
toonProp (Niet prop) | eqString (typeProp prop) "Var" = "!" ++ toonProp prop
                     | otherwise = "!(" ++ toonProp prop ++ ")"
toonProp (En []) = ""
toonProp (En (prop1:[])) | (eqString (typeProp prop1) "Of" || eqString (typeProp prop1) ":->") = "(" ++ toonProp prop1 ++ ")"
                         | otherwise = toonProp prop1
toonProp (En (prop1:props)) | (eqString (typeProp prop1) "Of" || eqString (typeProp prop1) ":->") = "(" ++ toonProp prop1 ++ ")" ++ " /\\ " ++ toonProp (En props)
                            | otherwise = toonProp prop1 ++ " /\\ " ++ toonProp (En props)
toonProp (Of []) = ""
toonProp (Of (prop1:[])) | eqString (typeProp prop1) ":->" = "(" ++ toonProp prop1 ++ ")"
                         | otherwise = toonProp prop1
toonProp (Of (prop1:props)) | eqString (typeProp prop1) ":->" = "(" ++ toonProp prop1 ++ ")" ++ " \\/ " ++ toonProp (En props)
                            | otherwise = toonProp prop1 ++ " \\/ " ++ toonProp (En props)
toonProp (prop1 :-> prop2) = toonProp prop1 ++ " -> " ++ toonProp prop2








typeProp :: Prop -> String
typeProp (Bool _) = "Bool"
typeProp (Var _) = "Var"
typeProp (Niet _) = "Niet"
typeProp (En _) = "En"
typeProp (Of _) = "Of"
typeProp (_ :-> _) = ":->"






multiLetters :: [Prop] -> Bedeling

multiLetters (prop1:[]) = letters (prop1)
multiLetters (prop1:props) = singleStrings (letters (prop1) ++ multiLetters (props))

singleStrings :: [String] -> [String]

singleStrings [] = []
singleStrings (x:[]) = [x]
singleStrings (x:xs) | (length(filter (eqString x) xs) == 0) = x : singleStrings xs
                     | otherwise = singleStrings xs

equivalent :: Prop -> Prop -> Bool
equivalent prop1 prop2 = eqList eqBool (map (evalueer prop1) (subs(multiLetters [prop1, prop2]))) (map (evalueer prop2) (subs(multiLetters [prop1, prop2])))



deMorgan :: Prop -> Prop
deMorgan (Bool prop) = (Bool prop)
deMorgan (Var letter) = (Var letter)
deMorgan (Niet prop) | (eqString (typeProp prop) "En" || eqString (typeProp prop) "Of") = deMorgan(deMorganConvert prop)
                     | otherwise = (Niet prop)
deMorgan (En []) = (En [])
deMorgan (En (prop1:[])) = (En ((deMorgan prop1):[]))
deMorgan (En (prop1:props)) = (En (prop1:props))
deMorgan (Of []) = (Of [])
deMorgan (Of (prop1:[])) = (Of ((deMorgan prop1):[]))
deMorgan (Of (prop1:props)) = (Of (prop1:props))
deMorgan (prop1 :-> prop2) = ((deMorgan prop1) :-> (deMorgan prop2))

deMorganConvert :: Prop -> Prop
deMorganConvert (En props) = (Of (nietConvert props))
deMorganConvert (Of props) = (En (nietConvert props))

nietConvert :: [Prop] -> [Prop]
nietConvert [Bool prop] = [Bool (not prop)]
nietConvert (prop1:[]) | eqString (typeProp prop1) "Bool" = nietConvert [prop1]
                       | otherwise = [Niet prop1]
nietConvert (prop1:props) | eqString (typeProp prop1) "Bool" = (nietConvert [prop1] : nietConvert props)
                          | otherwise = (Niet prop1 : nietConvert props)

-- ':' should be: '++'
-- 192,85-85
