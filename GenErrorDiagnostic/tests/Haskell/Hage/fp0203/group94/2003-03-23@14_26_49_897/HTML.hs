module HTML(module Pretty, module HTML) where

import Pretty

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
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
eerstVolgende (Tekst x) = x
eerstVolgende (SingleTag naam _)= naam
eerstVolgende (ClosedTag naam _ _) = naam

vergelijk1 :: [Requirement] -> (String,[String]) -> [String]
vergelijk1 [] _ = []
vergelijk1 ((x,y):ys) (naam,lijst) | eqString x naam = y : vergelijk1 ys (naam,lijst)
                                   | otherwise = vergelijk1 ys (naam,lijst)

vergelijk2 :: [Requirement] -> (String,[String]) -> Maybe Requirement
vergelijk2 _ (_, [])     = Nothing
vergelijk2 x (naam, (y:ys)) | elemBy eqString  y (vergelijk1 x (naam, (y:ys))) = vergelijk2 x (naam, ys)
                            | vergelijk3 x y = vergelijk2 x (naam, ys)
                            | otherwise = Just (naam,y)

vergelijk3 :: [Requirement] -> String -> Bool
vergelijk3 [] _              = True
vergelijk3 ((_,x):xs) y | not (eqString y x) = vergelijk3 xs y
                             | otherwise = False



ppHTML :: HTML -> Doc
ppHTML (Tekst x) = tekst x
ppHTML (SingleTag naam att) = tekst ("<" ++ naam ++ attributen att ++ ">")
ppHTML (ClosedTag naam att htmls) | simpel htmls = tekst ("<" ++ naam ++ attributen att ++ ">") <|> ppHTML2 htmls <|> tekst ("</" ++ naam ++ ">")
                                  | otherwise = tekst ("<" ++ naam ++ attributen att ++ ">") <-> springIn 1 (ppHTML3 htmls) <-> tekst ("</" ++ naam ++ ">")

ppHTML2 :: [HTML] -> Doc
ppHTML2 [] = []
ppHTML2 (x:xs) = ppHTML x <|> ppHTML2 xs

ppHTML3 :: [HTML] -> Doc
ppHTML3 [] = []
ppHTML3 (x:xs) = ppHTML x <-> ppHTML3 xs

attributen :: [String] -> String
attributen []     = ""
attributen (x:xs) = " " ++ x ++ attributen xs

simpel :: [HTML] -> Bool
simpel [] = True
simpel ((Tekst _) : dinges) = simpel dinges
simpel ((SingleTag _ _) : dinges) = simpel dinges
simpel ((ClosedTag _ _ (html:[])): dinges) = simpel [html] && simpel dinges
simpel ((ClosedTag _ _ _): _) = False



zooi :: Int -> Char
zooi x = ["A","B","C","D","E","F"] !! (x-10)


-- signature should be Int -> String
-- 88,1-4   88,9-19   88,16-19
