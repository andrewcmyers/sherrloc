module HTML(module HTML, module Pretty) where

import Pretty
elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqMaybe :: (a -> a -> Bool) -> (Maybe a) -> (Maybe a) -> Bool
eqMaybe = undefined
eqTuple2 :: (a -> a -> Bool) -> (b -> b -> Bool) -> ((a,b) -> (a,b) -> Bool)
eqTuple2 = undefined
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







type Attribute  = Attrib
type Attributes = [Attrib]

color :: Int -> Int -> Int -> String
color r g b = "#" ++ hexadecimaal r ++ hexadecimaal g ++ hexadecimaal b
            where hexadecimaal x = [kleur !! (x/16)] ++ [kleur !! (x `mod` 16)]
                  kleur = "0123456789ABCDEF"

colorTable :: [[(Int, Int, Int)]] -> HTML
colorTable kleuren = TagDubbel (Open "TABLE")
                               [Attrib "HEIGHT" "400", Attrib "WIDTH" "400"]
                               [TagDubbel (Open "TBODY") []
                               (rijen kleuren)
                               (Sluit "/TBODY")]
                               (Sluit "/TABLE")

rijen :: [[(Int, Int , Int)]] -> [HTML]
rijen (rij:[])   = [TagDubbel (Open "TR") [] (cellen rij) (Sluit "/TR")]
rijen (rij:rijs) = [TagDubbel (Open "TR") [] (cellen rij) (Sluit "/TR")] ++ rijen rijs

cellen :: [(Int, Int, Int)] -> [HTML]
cellen ((r,g,b):[])      = [TagDubbel (Open "TD") [Attrib "BGCOLOR" (color r g b)] [] (Sluit "/TD")]
cellen ((r,g,b):kleuren) = [TagDubbel (Open "TD") [Attrib "BGCOLOR" (color r g b)] [] (Sluit "/TD")] ++ cellen kleuren

ul         :: [Attributes] -> [[HTML]] -> HTML
ul attrib html = TagDubbel (Open "UL") [] (li attrib html) (Sluit "/UL")

li         :: [Attributes] -> [[HTML]] -> [HTML]
li               [] (html:htmls) = [TagDubbel (Open "LI") [] html(Sluit "/LI")] ++ (li [] htmls)
li               _     (html:[]) = [TagDubbel (Open "LI") [] html(Sluit "/LI")]
li (attrib:attribs) (html:htmls) = [TagDubbel (Open "LI") [attrib] html(Sluit "/LI")] ++ (li attribs htmls)










h          :: Int -> String -> HTML
h grootte text' = TagDubbel (Open ("H" ++ grootteS)) [] [Tekst text'] (Sluit ("/H"++grootteS))
           where grootteS = showInt grootte

font       :: [Attribute] -> [HTML] -> HTML
font attribuut html = TagDubbel (Open "FONT") attribuut html (Sluit "/FONT")

document   :: String -> [HTML] -> HTML
document titel body = TagDubbel (Open "HTML") [] [
                      TagDubbel (Open "HEAD") [] [
                      TagDubbel (Open "TITLE")[] [
                      Tekst titel]
                      (Sluit "/TITLE")]
                      (Sluit "/HEAD"),
                      TagDubbel (Open "BODY") []
                      body
                      (Sluit "/BODY")]
                      (Sluit "/HTML")

text       :: String -> HTML
text text' = (Tekst text')



kleurenTabel :: HTML
kleurenTabel = document "Kleurentabel" [
                (h 1 "Kleurentabel")
               ,(ul []
                [[font [Attrib "COLOR" (color 255 0 0)] [text "Rood"], text "loopt van boven naar onder van 0 tot en met 250 in stappen van 25"]
                ,[font [Attrib "COLOR" (color 0 255 0)] [text "Groen"], text "loopt van rechts naar links van 0 tot en met 250 in stappen van 25"]
                ,[font [Attrib "COLOR" (color 0 0 255)] [text "Blauw"], text "is overal 0"]])]








model :: HTML
model =  TagDubbel (Open "HTML") []
        [TagDubbel (Open "HEAD") []
        [TagDubbel (Open "FONT") [Attrib "COLOR" "#0000FF"]
        [Tekst "Model"]
         (Sluit "/FONT")]
         (Sluit "/HEAD")
        ,TagDubbel (Open "BODY") []
        [TagEnkel  "HR"          []
        ,TagDubbel (Open "UL")   []
        [TagDubbel (Open "LI")   []
        [TagDubbel (Open"FONT") [Attrib "COLOR" "#0000FF"]
        [Tekst "Eerste "]
         (Sluit "/FONT")
        ,Tekst "punt"]
         (Sluit "/LI")
        ,TagDubbel (Open "LI")   []
        [Tekst "Tweede punt"]
        (Sluit "/LI")]
        (Sluit "/UL")
        ,TagEnkel "HR"           []]
         (Sluit "/BODY")]
         (Sluit "/HTML")


requirements :: [Requirement]
requirements = [("UL","LI"),("HTML","HEAD")
               ,("HTML","BODY"),("BODY","TABLE")
               ,("BODY","H1"),("BODY","H2")
               ,("BODY","H3"),("BODY","H4")
               ,("BODY","H5"),("BODY","H6")
               ,("BODY","HR")
               ]





makeKleurL :: Int -> Int -> [[(Int,Int,Int)]]
makeKleurL aantal stap = map (makeKleur aantal stap) [0,stap..(aantal*stap)]

makeKleur :: Int -> Int -> Int -> [(Int,Int,Int)]
makeKleur aantal stap r = [(r , groen , 0) | groen <- [0,stap..(aantal*stap)]]

-- [attrib] should be: attrib
-- 90,60-65
