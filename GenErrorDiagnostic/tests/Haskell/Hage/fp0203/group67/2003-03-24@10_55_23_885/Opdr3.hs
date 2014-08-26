module Opdr3(module Pretty, module Opdr3) where

import Pretty

data HTML = Tekst String
          | Enkel Tag HTML
          | Dubbel Tag [HTML]

eqString      :: String -> String -> Bool 
eqString = undefined

type Tag = (Naam,[Arg])
type Naam = String
type Arg = (Attr,Waarde)
type Attr = String
type Waarde = String

type Requirement = (String, String)

validate :: [Requirement] -> HTML -> Maybe Requirement

validate [] _ = Nothing
validate (req:reqs) html | check req html = validate reqs html
                         | otherwise = Just req

check :: Requirement -> HTML -> Bool
check _ (Tekst _) = True
check req (Enkel _ html) = check req html
check req (Dubbel tag htmls) | eqString (fst req) (fst tag) = True
                             | eqString (snd req) (fst tag) = False
                             | otherwise = and (map (check req) htmls)

ppHTML :: HTML -> Doc
ppHTML html = lines (ppString html)

ppString :: HTML -> String
ppString html | isSimpel html = ppSimpel html
              | otherwise = ppNietSimpel html

ppSimpel :: HTML -> String
ppSimpel (Tekst s) = s
ppSimpel (Enkel tag html) = ppTag tag ++ " " ++ ppString html
ppSimpel (Dubbel tag html) = ppTag tag ++ " " ++ ppStrings html ++ " </" ++ (fst tag) ++ ">"

ppStrings :: [HTML] -> String
ppStrings [] = ""
ppStrings (html:htmls) = (ppString html) ++ (ppStrings htmls)

ppNietSimpel :: HTML -> String
ppNietSimpel (Dubbel tag htmls) = ([(ppTag tag ++ "\n ")] <|> [ppStrings htmls]) ++ "\n</" ++ (fst tag) ++ ">"

ppTag :: Tag -> String
ppTag (naam,args) = "<" ++ naam ++ (ppArg args) ++ ">"

ppArg :: [Arg] -> String
ppArg [] = ""
ppArg ((attr,waarde):args) = " " ++ attr ++ "=" ++ "\"" ++ waarde ++ "\"" ++ " " ++ ppArg args

isSimpel :: HTML -> Bool
isSimpel (Dubbel _ [_]) = True
isSimpel (Tekst _) = True
isSimpel (Enkel _ _) = True
isSimpel _ = False


goed :: HTML
goed = Enkel ("BR",[]) (Dubbel ("UL",[]) [(Dubbel ("IL",[]) [(Tekst "bla")])])
fout :: HTML
fout = Enkel ("BR",[]) (Dubbel ("IL",[]) [(Dubbel ("UL",[]) [(Tekst "bla")])])

-- should be: unwords (...)
-- 50,36-79
