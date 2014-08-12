module HTML(module Pretty, module HTML) where

import Pretty
eqString      :: String -> String -> Bool 
eqString = undefined

data HTML
     = SingleTag String [Attributen]
     | ClosedTag String [Attributen] [HTML]
     | Tekst String

type Attributen = String
type Requirement = (String, String)


getTagName :: HTML -> String
getTagName (ClosedTag name _ _) = name
getTagName (SingleTag name _) = name
getTagName (Tekst _) = ""

validate1 :: Requirement -> HTML -> Bool
validate1 (a,b) (ClosedTag naam att (x:xs)) |(eqString (getTagName x) b) = (eqString a naam) && (validate1 (a,b) x) && (validate1 (a,b) (ClosedTag naam att xs))
                                            |otherwise = (validate1 (a,b) x) && (validate1 (a,b) (ClosedTag naam att xs))
validate1 _ _ = True

validate :: [Requirement] -> HTML -> Maybe Requirement
validate (tuple:xs) naam | not(eqString (snd tuple) (getTagName naam)) = if (validate1 tuple naam) then
                                                                               validate xs naam
                                                                         else
                                                                               Just tuple
                         | otherwise = Just tuple
validate [] _ = Nothing


simple :: HTML -> Bool
simple (ClosedTag _ _ x) = doorloopElementen x
simple _ = True

controleerElement :: HTML -> Bool
controleerElement (Tekst _) = True
controleerElement (ClosedTag _ _ [_]) = True
controleerElement (SingleTag _ _) = True
controleerElement _ = False

doorloopElementen :: [HTML] -> Bool
doorloopElementen (x:xs) = controleerElement x && doorloopElementen xs
doorloopElementen [] = True

prepAttr :: [Attributen] -> Doc
prepAttr [] = tekst ""
prepAttr attributen = tekst " " <|> unwords attributen

ppHTML :: HTML -> Doc
ppHTML (ClosedTag name att html) | not(doorloopElementen html) = tekst "<" <|> tekst name <|> prepAttr att <|> tekst ">"
                                                                 <-> springIn 2 (ppLijstHTML html)
                                                                 <-> tekst "</" <|> tekst name <|> tekst ">"
                                 | otherwise = tekst "<" <|> tekst name <|> prepAttr att <|> tekst ">"
                                               <|> (ppLijstHTML html) <|> tekst "</" <|> tekst name <|> tekst ">"
ppHTML (SingleTag name att) = tekst "<" <|> tekst name <|> tekst (unwords att) <|> tekst ">"
ppHTML (Tekst woorden) = tekst woorden

ppLijstHTML :: [HTML] -> Doc
ppLijstHTML (x:xs) = ppHTML x <|> ppLijstHTML xs
ppLijstHTML [] = tekst ""

-- unwords attributen should be: tekst (unwords attributen)
-- 51,37-54
