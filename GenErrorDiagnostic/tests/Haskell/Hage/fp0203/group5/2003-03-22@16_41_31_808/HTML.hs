module HTML(module Pretty, module HTML) where

import Pretty
eqString      :: String -> String -> Bool 
eqString = undefined
elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqMaybe :: (a -> a -> Bool) -> (Maybe a) -> (Maybe a) -> Bool
eqMaybe = undefined

eqTuple2 :: (a -> a -> Bool) -> (b -> b -> Bool) -> ((a,b) -> (a,b) -> Bool)
eqTuple2 = undefined

data HTML = Tekst String
          | SingleTag String [Att]
          | ClosedTag String [Att] [HTML]

type Att = String
type Requirement = (String, String)


validate :: [Requirement] -> HTML -> Maybe Requirement
validate _ (Tekst _)          = Nothing
validate _ (SingleTag _ _)      = Nothing
validate x (ClosedTag naam _ htmls) | eqMaybe (eqTuple2 eqString eqString) Nothing (vergelijk2 x (naam,onderliggendeTags htmls)) = validate2 x htmls
                                    | otherwise = vergelijk2 x (naam,onderliggendeTags htmls)

validate2 :: [Requirement] -> [HTML] -> Maybe Requirement
validate2 _ []                    = Nothing
validate2 x (y:ys) | eqMaybe (eqTuple2 eqString eqString) Nothing (validate x y) = validate2 x ys
                   | otherwise = validate x y

onderliggendeTags :: [HTML] -> [String]
onderliggendeTags [] = []
onderliggendeTags (x:xs) = eerstVolgende x : onderliggendeTags xs

eerstVolgende :: HTML -> String
eerstVolgende (Tekst _) = ""
eerstVolgende (SingleTag naam _)= naam
eerstVolgende (ClosedTag naam _ _) = naam

vergelijk1 :: [Requirement] -> (String,[String]) -> [String]
vergelijk1 [] _ = []
vergelijk1 ((x,y):ys) (naam,lijst) | eqString x naam = y : vergelijk1 ys (naam,lijst)
                                   | otherwise = vergelijk1 ys (naam,lijst)

vergelijk2 :: [Requirement] -> (String,[String]) -> Maybe Requirement
vergelijk2 _ (_, [])     = Nothing
vergelijk2 x (naam, (y:ys)) | elemBy eqString  y (vergelijk1 x (naam, (y:ys))) = vergelijk2 x (naam, ys)
                            | otherwise = Just (naam,y)





ppHTML :: HTML -> Doc
ppHTML (Tekst x) = [x]
ppHTML (SingleTag naam att) = ["<" ++ naam ++ attributen att ">"]

attributen :: [String] -> String
attributen []     = ""
attributen (x:xs) = " " ++ x ++ attributen xs

-- should be: attributen att ++ ">"
-- 58,47-64
