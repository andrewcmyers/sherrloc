module HTML3 where

import Pretty

eqString      :: String -> String -> Bool 
eqString = undefined
chr :: Int -> Char
chr = undefined
showInt       :: Int -> String
showInt = undefined
toUpper :: Char -> Char
toUpper = undefined

data HTML = Text String
          | SingleTag String [Attribute]
          | DoubleTag String [Attribute] [HTML]

type Attribute = (String,String)

voorbeeld :: HTML
voorbeeld = DoubleTag "BODY" [("color","blue")] [(Text "tekst"),(SingleTag "PRE" [("color","red")]),(SingleTag "PRE" [("color","blue")])]



type Requirement = (String, String)

valideer :: HTML -> Requirement -> Maybe Requirement
valideer (Text _ ) _         = Nothing
valideer (SingleTag _ _ ) _  = Nothing
valideer (DoubleTag tag _ htmls) req | eqString tag (snd req) = Just req
                                     | eqString tag (fst req) = Nothing
                                     | otherwise = dunUit (map (\html -> valideer html req) htmls)

validate :: [Requirement] -> HTML -> Maybe Requirement
validate reqs html = dunUit (map (valideer html) reqs)







dunUit :: [Maybe Requirement] -> Maybe Requirement
dunUit (Nothing:reqs) = dunUit reqs
dunUit []             = Nothing
dunUit (Just a:_)     = Just a

regels :: [Requirement]
regels = [("UL", "LI"), ("HTML", "BODY"), ("HTML", "HEAD"),
          ("BODY", "H"), ("BODY", "HR"), ("BODY", "TABLE")]



ppHTML :: HTML -> Doc
ppHTML (Text string)              = tekst string
ppHTML (SingleTag tag atts )      = tekst (toon (SingleTag tag atts))
ppHTML (DoubleTag tag atts htmls) | length htmls == 1 = tekst (toon (DoubleTag tag atts htmls))
                                  | otherwise         = tekst ("<" ++ tag ++ toonAtts atts ++ ">") <-> (foldr (<->) leeg (map (springIn 1) (map ppHTML htmls))) <-> tekst ("</" ++ tag ++ ">")






toon :: HTML -> String
toon (Text string)              = string
toon (SingleTag tag atts)       = "<" ++ tag ++ toonAtts atts ++ ">"
toon (DoubleTag tag atts htmls) = "<" ++ tag ++ toonAtts atts ++ ">" ++ concatMap toon htmls ++ "</" ++ tag ++ ">"

toonAtts :: [(String, String)] -> String
toonAtts atts = concat (map (' ':) (map plakVast atts))
                                  where plakVast att = fst att ++ "=" ++ "\"" ++ snd att ++ "\""





color :: Int -> Int -> Int -> String
color a b c = "#" ++ hexaDec a ++ hexaDec b ++ hexaDec c
            where hexaDec getal = intLetter (getal / 16) ++ intLetter (getal `rem` 16)

intLetter :: Int -> String
intLetter getal | getal < 10 = (chr (getal + 48)) : ""
                | otherwise  = (toUpper(chr (getal + 87))) : ""







colorTable :: [[(Int, Int, Int)]] -> HTML
colorTable colorss = DoubleTag "TABLE" [("BORDER","1"),("WIDTH","400"),("HEIGHT","400")] (map maakRij colorss)

maakRij :: [(Int, Int, Int)] -> HTML
maakRij colors = DoubleTag "TR" [] (map maakVeld colors)

maakVeld :: (Int, Int, Int) -> HTML
maakVeld (a, b, c) = DoubleTag "TD" [("color",color a b c)] []

ul :: [[HTML]] -> HTML
ul htmls = DoubleTag "UL" [] (map li htmls)

li :: [HTML] -> HTML
li htmls = DoubleTag "LI" [] htmls

h :: Int -> String -> HTML
h grootte string = DoubleTag ("H" ++ showInt grootte) [] [Text string]

font :: [Attribute] -> [HTML] -> HTML
font attrs htmls = DoubleTag "FONT" attrs htmls

text :: String -> HTML
text string = Text string

document :: String -> [HTML] -> HTML
document titel htmls = DoubleTag "HTML" [] [DoubleTag "HEAD" [] [DoubleTag "TITLE" [] [Text titel] ], DoubleTag "BODY" [] htmls ]

kleurenTabel :: HTML
kleurenTabel = document "Kleurentabel" [(h 1 "Kleurentabel"),
                                       (ul [[font [("COLOR","#FF0000")] [text "Rood"], text "loopt van boven naar onder van 0 tot en met 250 in stappen van 25"],[font [("COLOR","#00FF00")] [text "Groen"], text "loopt van boven naar onder van 0 tot en met 250 in stappen van 25"],[font ["COLOR","#0000FF"] [text "Blauw"], text "is overal 0"]])]

-- missing () around: "COLOR","#0000FF"
-- 121,287-293   121,295-303   121,287-303
