module HTML6(module HTML6, module Pretty) where
import Pretty
eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined
data HTML = Tekst String
           | EnkeleTag Name [(Name, Value)]
           | TweeTag Name [(Name,Value)] [HTML]
type Name  = String
type Value = String



example :: HTML
example = TweeTag "UL" [] [TweeTag "LI" [] [Tekst "12"], TweeTag "LI" [][Tekst "3"]]











type Requirement = (String, String)







validate :: [Requirement] -> HTML -> Maybe Requirement
validate []_ = Nothing
validate ((req,con):xs) html |not (eqBool (isGelijkCheck req con html) True) = Just (req,con)
                             |otherwise = validate xs html





isGelijkCheck :: String -> String -> HTML -> Bool
isGelijkCheck req con html = (beginCheckDirectBinnen req con html)==(beginTelCons con html)




beginCheckDirectBinnen :: String -> String -> HTML -> Int
beginCheckDirectBinnen _ _(Tekst _) = 0
beginCheckDirectBinnen _ _ (EnkeleTag _ _) = 0
beginCheckDirectBinnen req con (TweeTag req1 _ html)      |eqString req req1= telVK con html + checkDirectBinnen req con html
                                                          |otherwise = checkDirectBinnen req con html

checkDirectBinnen :: String -> String -> [HTML] -> Int
checkDirectBinnen _ _ [] = 0
checkDirectBinnen req con ((Tekst _):xs) = 0 + checkDirectBinnen req con xs
checkDirectBinnen req con ((EnkeleTag _ _):xs) = 0 + checkDirectBinnen req con xs
checkDirectBinnen req con ((TweeTag req1 _ html):xs)      |eqString req req1= telVK con html +checkDirectBinnen req con html + checkDirectBinnen req con xs
                                                          |otherwise = checkDirectBinnen req con html + checkDirectBinnen req con xs


beginTelCons :: String -> HTML -> Int
beginTelCons con (TweeTag con1 _ html) |eqString con con1 = 1+telCons con html
                                       |otherwise = telCons con html
beginTelCons con (EnkeleTag con1 _)    |eqString con con1 = 1
                                       |otherwise = 0
beginTelCons _ (Tekst _)= 0

telCons :: String -> [HTML] -> Int
telCons _ []= 0
telCons con ((TweeTag con1 _ html):xs)    |eqString con con1 = 1+ telCons con xs + telCons con html
                                          |otherwise = telCons con xs + telCons con html

telCons con ((Tekst _):xs) = 0 + telCons con xs
telCons con ((EnkeleTag con1 _):xs) |eqString con con1 = 1 + telCons con xs
                                    |otherwise = 0 +telCons con xs

telVK :: String -> [HTML] -> Int
telVK _ []= 0
telVK con ((TweeTag con1 _ _):xs)       |eqString con con1 = 1+ telVK con xs
                                        |otherwise = telVK con xs
telVK con ((Tekst _):xs) = 0 + telVK con xs
telVK con ((EnkeleTag con1 _):xs)  |eqString con con1 = 1+ telVK con xs
                                   |otherwise = telVK con xs













ppHTML :: HTML -> Doc
ppHTML (Tekst html) = tekst html
ppHTML (EnkeleTag tagName ats) = ppTag tagName ats
ppHTML (TweeTag tagName ats html)    | not (simpel (TweeTag tagName ats html)) = ppTag tagName ats <-> (springIn 1 ppLijstHTML html) <-> ppEndTag tagName
                                     | otherwise = ppTag tagName ats <|> ppLijstHTML html <|> ppEndTag tagName


ppTag :: String -> [(Name, Value)] -> Doc
ppTag tagName [] = tekst "<" <|> tekst tagName <|> tekst ">"
ppTag tagName ats = tekst "<" <|> tekst tagName <+> ppNameValueList ats <|> tekst ">"

ppLijstHTML :: [HTML] -> Doc
ppLijstHTML [] = leeg
ppLijstHTML (x:xs) = ppHTML x <-> ppLijstHTML xs

ppEndTag :: String -> Doc
ppEndTag tagName = tekst "</" <|> tekst tagName <|> tekst ">"

ppNameValueList :: [(Name, Value)] -> Doc
ppNameValueList [] = leeg
ppNameValueList ((naam, waarde): ats) = tekst naam <|> tekst "=" <|> tekst "\"" <|> tekst waarde <|> tekst "\""<+> (ppNameValueList ats)

simpel :: HTML -> Bool
simpel (Tekst _) = True
simpel (EnkeleTag _ _)= True
simpel (TweeTag _ _ html)   = allTrue (map simpel html)

allTrue :: [Bool] -> Bool
allTrue []= True
allTrue (x:xs) = x&& allTrue xs

-- missing parenthesis
-- 104,116-131
