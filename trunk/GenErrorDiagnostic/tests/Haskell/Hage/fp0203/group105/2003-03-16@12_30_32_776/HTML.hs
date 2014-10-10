module HTML where

import Pretty
elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqMaybe :: (a -> a -> Bool) -> (Maybe a) -> (Maybe a) -> Bool
eqMaybe = undefined
eqTuple2 :: (a -> a -> Bool) -> (b -> b -> Bool) -> ((a,b) -> (a,b) -> Bool)
eqTuple2 = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
showInt       :: Int -> String
showInt = undefined
data HTML = Tekst String
          | TagEnkel  Naam [Attrib]
          | TagDubbel Open [Attrib] [HTML] Sluit
data Open = Open Naam
data Sluit= Sluit Naam
data Attrib = Attrib Naam Waarde
type Naam = String
type Waarde = String
type Requirement = (String, String)
validate   :: [Requirement] -> HTML -> Maybe Requirement
validate                     []    _ = Nothing
validate ((binnen,buiten):eiss) html | eqMaybe (eqTuple2 (eqString) (eqString)) (Just(binnen,buiten))(testEis (getStrings html) (binnen,buiten)) = Just (binnen,buiten)
                                     | otherwise = validate eiss html
getStrings :: HTML -> [String]
getStrings (Tekst _) = []
getStrings (TagEnkel tagEnkel _) = [tagEnkel]
getStrings (TagDubbel (Open open) _ htmls (Sluit sluit)) = [open] ++ lijst ++ [sluit]
           where lijst = concat(map getStrings htmls)

testEis    :: [String] -> Requirement -> Maybe Requirement
testEis html (buiten,binnen) | elemBy eqString binnen html2 = Just (buiten,binnen)
                             | otherwise = Nothing
           where html2 = del buiten binnen html

del :: String -> String -> [String] -> [String]
del _ _ [] = []
del buiten binnen lijst = (takeWhile (not.eqString buiten) lijst) ++
                          dels ++
                          (dropWhile (not.eqString ('/':buiten)) lijst)
           where dels = filter (not.eqString binnen)
                        (takeWhile (not.eqString ('/':buiten))
                        (dropWhile (not.eqString buiten) lijst))












ppHTML     :: HTML -> Doc
ppHTML (Tekst regel) = [regel]
ppHTML (TagEnkel naam attribs) = ["<"++naam++ printAttribs attribs ++">"]
ppHTML (TagDubbel (Open naam) attribs html (Sluit _))
                | simpel html = ["<"++naam++ printAttribs attribs ++">"] <|>
                                printHTML html <|>
                                ["</"++naam++">"]
                | otherwise   = ["<"++naam++ printAttribs attribs ++">"] <->
                                springIn 1 (printHTML html) <->
                                ["</"++naam++">"]
                where simpel [] = True
                      simpel [(Tekst _):xs] = simpel xs
                      simpel [(TagEnkel _ _):xs] = simpel xs
                      simpel [(TagDubbel _ _ html' _):xs] = (length html' <= 1) && simpel html' && simpel xs
                      simpel _ = False








printHTML :: [HTML] -> Doc
printHTML [] = []
printHTML (html:htmls)                   = ppHTML html <-> printHTML htmls




printAttribs :: [Attrib] -> String
printAttribs []     = ""
printAttribs ((Attrib naam waarde):xs) = " " ++ naam ++ "=\"" ++ waarde ++ "\"" ++ printAttribs xs






type Attribute  = Attrib
type Attributes = [Attrib]

color :: Int -> Int -> Int -> String
color r g b = "#" ++ hexadecimaal r ++ hexadecimaal g ++ hexadecimaal b
            where hexadecimaal x = [kleur !! (x/16)] ++ [kleur !! (x `mod` 16)]
                  kleur = "0123456789ABCDEF"

colorTable :: [[(Int, Int, Int)]] -> HTML
colorTable kleuren = TagDubbel (Open "TABLE")
                               [Attrib "HEIGHT" "400", Attrib "WIDTH" "400"]
                               (rijen kleuren)
                               (Sluit "TABLE")

rijen :: [[(Int, Int , Int)]] -> [HTML]
rijen (rij:[])   = [TagDubbel (Open "TR") [] (cellen rij) (Sluit "TR")]
rijen (rij:rijs) = [TagDubbel (Open "TR") [] (cellen rij) (Sluit "TR")] ++ rijen rijs

cellen :: [(Int, Int, Int)] -> [HTML]
cellen ((r,g,b):[])      = [TagDubbel (Open "TD") [Attrib "BGCOLOR" (color r g b)] [] (Sluit "TD")]
cellen ((r,g,b):kleuren) = [TagDubbel (Open "TD") [Attrib "BGCOLOR" (color r g b)] [] (Sluit "TD")] ++ cellen kleuren

ul         :: [Attributes] -> [HTML] -> HTML
ul attrib html = TagDubbel (Open "UL") [] (li attrib html) (Sluit "UL")

li         :: [Attributes] -> [HTML] -> [HTML]
li (attrib:[]) (html:[]) = [TagDubbel (Open "LI") attrib [html] (Sluit "LI")]
li (attrib:attribs) (html:htmls) = [TagDubbel (Open "LI") attrib [html] (Sluit "LI")] ++ (li attribs htmls)

h          :: Int -> String -> HTML
h grootte text' = TagDubbel (Open ("H" ++ grootteS)) [] [Tekst text'] (Sluit ("H"++grootteS))
           where grootteS = showInt grootte

font       :: [Attribute] -> [HTML] -> HTML
font attribuut html = TagDubbel (Open "FONT") attribuut html (Sluit "FONT")

document   :: String -> [HTML] -> HTML
document titel body = TagDubbel (Open "HTML") [] [
                      TagDubbel (Open "HEAD") [] [
                      TagDubbel (Open "TITLE")[] [
                      Tekst titel]
                      (Sluit "TITLE")]
                      (Sluit "HEAD"),
                      TagDubbel (Open "BODY") []
                      body
                      (Sluit "BODY")]
                      (Sluit "HTML")

text       :: String -> HTML
text text' = (Tekst text')











model1 :: HTML
model1 =  TagDubbel (Open "HTML") []
         [TagDubbel (Open "HEAD") []
         [TagDubbel (Open "FONT") [Attrib "COLOR" "#0000FF"]
         [Tekst "Model"]
          (Sluit "FONT")]
          (Sluit "HEAD")
         ,TagDubbel (Open "BODY") []
         [TagEnkel  "HR"          []
         ,TagDubbel (Open "UL")   []
         [TagDubbel (Open "LI")   []
         [TagDubbel (Open"FONT") [Attrib "COLOR" "#0000FF"]
         [Tekst "Eerste "]
          (Sluit "FONT")
         ,Tekst "punt"]
          (Sluit "LI")
         ,TagDubbel (Open "LI")   []
         [Tekst "Tweede punt"]
         (Sluit "LI")]
         (Sluit "UL")
         ,TagEnkel "HR"           []]
          (Sluit "BODY")]
          (Sluit "HTML")

model1a :: HTML
model1a =  TagDubbel (Open "LI") []
          [TagDubbel (Open"FONT") [Attrib "COLOR" "#0000FF"]
          [Tekst "Eerste "]
           (Sluit "FONT")
          ,Tekst "punt"]
           (Sluit "LI")







model2 :: HTML
model2 =  TagDubbel (Open "UL") []
         [TagDubbel (Open "LI") []
         [Tekst "12"] (Sluit "LI")
         ,TagDubbel (Open "LI") []
         [Tekst "3"] (Sluit "LI")]
          (Sluit "UL")


model3 :: HTML
model3 =  TagDubbel (Open "LI") []
         [Tekst "12"] (Sluit "LI")


model4 :: HTML
model4 =  TagDubbel (Open "EM") []
         [TagDubbel (Open "B") []
         [Tekst "tekst"] (Sluit "B")]
          (Sluit "EM")

requirements :: [Requirement]
requirements = [("UL","LI"),("HTML","HEAD")
               ,("HTML","BODY"),("BODY","TABLE")
               ,("BODY","H1"),("BODY","H2")
               ,("BODY","H3"),("BODY","H4")
               ,("BODY","H5"),("BODY","H6")
               ,("BODY","HR")
               ]

-- [] should be ()
-- 71,31-42  72,31-47  73,31-56   72:45-45   73,54-54
