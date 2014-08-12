module HTML(module HTML, module Pretty) where
import Pretty

eqString      :: String -> String -> Bool 
eqString = undefined
chr :: Int -> Char
chr = undefined

data HTML = Tekst String
          | EnkeleTag Tag [Attribuut]
          | GeslotenTag Tag [Attribuut] [HTML]


type Tag         = String
type Attribuut   = (Naam, Waarde)
type Naam        = String
type Waarde      = String
type Requirement = (String, String)













vbHTML :: HTML
vbHTML = GeslotenTag "UL" [][li1, hr, li2]

li1 :: HTML
li1 = GeslotenTag "LI" [] [GeslotenTag "FONT" [("COLOR","#0000FF")] [Tekst "Eerste"]
                          ,Tekst "punt"
                          ]

hr :: HTML
hr = EnkeleTag "HR" []

li2 :: HTML
li2 = GeslotenTag "LI" [] [Tekst "Tweede punt"]





validate :: [Requirement] -> HTML -> Maybe Requirement
validate [] _                              = Nothing
validate (r:req) html | checkTag r html [] = validate req html
                      | otherwise          = Just r




checkTag :: Requirement -> HTML -> String -> Bool
checkTag _                  (Tekst _)          _                                         = True
checkTag _                  (EnkeleTag _ _)    _                                         = True
checkTag (ouderreq,kindreq) (GeslotenTag tag _ htmls) string   | tag `eqString` kindreq  = ouderreq `eqString` string
                                                               | otherwise               = and (map (checkKind) htmls)
                                                                   where
                                                                   checkKind html        = checkTag (ouderreq, kindreq) html tag





vbRequirement :: [Requirement]
vbRequirement = [vbRequirement1, vbRequirement2, vbRequirement3, vbRequirement4, vbRequirement5, vbRequirement6, vbRequirement7]
vbRequirement1 :: Requirement
vbRequirement1 = ("UL", "LI")
vbRequirement2 :: Requirement
vbRequirement2 = ("HTML", "BODY")
vbRequirement3 :: Requirement
vbRequirement3 = ("HTML", "HEAD")
vbRequirement4 :: Requirement
vbRequirement4 = ("BODY", "Table")
vbRequirement5 :: Requirement
vbRequirement5 = ("BODY", "H")
vbRequirement6 :: Requirement
vbRequirement6 = ("BODY", "HR")
vbRequirement7 :: Requirement
vbRequirement7 = ("LI", "FONT")

















ppHTML :: HTML -> Doc
ppHTML (Tekst string)                           = tekst string
ppHTML (EnkeleTag tag atts)                     = openTag tag atts

ppHTML (GeslotenTag tag atts (htmls))
        | simpel (GeslotenTag tag atts (htmls)) = openTag tag atts <|> horizontaleLijst(map ppHTML htmls) <|> closeTag tag
        | otherwise                             = openTag tag atts <-> springIn 1 (verticaleLijst(map ppHTML htmls)) <-> closeTag tag


openTag :: Tag -> [Attribuut] -> Doc
openTag tag atts = tekst ("<" ++ tag ++ (concatMap attrNaarStr atts) ++ ">")


closeTag :: Tag -> Doc
closeTag tag = tekst("</" ++ tag ++ ">")


horizontaleLijst :: [Doc] -> Doc
horizontaleLijst []       = leeg
horizontaleLijst (x:xs)   = x <|> horizontaleLijst xs



simpel :: HTML -> Bool
simpel (Tekst _)                                 = True
simpel (EnkeleTag _ _)                           = True
simpel (GeslotenTag _ _ yys)                     = and(map simpelElement yys)






simpelElement :: HTML -> Bool
simpelElement (Tekst _)                          = True
simpelElement (EnkeleTag _ _)                    = True
simpelElement (GeslotenTag _ _[x])               = simpelElement x
simpelElement _                                  = False


attrNaarStr :: Attribuut -> String
attrNaarStr (naam, waarde) = " " ++ naam ++ "="++ "\"" ++ waarde ++ "\""







vb1 :: HTML
vb1 = GeslotenTag "EM" [] [GeslotenTag "B" [] [Tekst "tekst"]
                          ,Tekst "je"
                          ]







vb2 :: HTML
vb2 = GeslotenTag "UL" [] [GeslotenTag "LI" [] [GeslotenTag "B" [] [Tekst "1"]
                                               ,Tekst "12"
                                               ]
                          ,GeslotenTag "LI" [] [Tekst "3"]
                          ]






color :: Int -> Int -> Int -> String
color n1 n2 n3 = "#" ++ (concatMap int2HexStr [n1, n2, n3])


int2HexStr :: Int -> String
int2HexStr = hex2HexStr . hexValue


hex2HexStr :: (Int, Int) -> String
hex2HexStr (hex1, hex2) = (hex2Str hex1) ++ (hex2Str hex2)


hexValue :: Int -> (Int, Int)
hexValue n = (((n - mod16 n)/16), mod16 n)
 where mod16 x = (x `mod` 16)


hex2Str :: Int -> String
hex2Str n | n >= 10     = [chr (n + 55)]
          | otherwise   = [chr (n + 48)]



colorTable :: [[(Int, Int, Int)]] -> HTML
colorTable xs = GeslotenTag "TABLE" [("WIDTH","400"),("HEIGHT","400")] (map colorRow xs)


colorRow :: [(Int, Int, Int)] -> HTML
colorRow xs = GeslotenTag "TR" [] (map colorElement xs)


colorElement :: (Int, Int, Int) -> HTML
colorElement (x,y,z) = GeslotenTag "TD" [("BGCOLOR", (color x y z))] []


ul :: [Attribuut] -> [[HTML]] -> HTML
ul attributen htmls = GeslotenTag "UL" attributen (map li) htmls


li :: [HTML] -> HTML
li xxs = GeslotenTag "LI" [] xxs


h :: Int -> String -> HTML
h n string = GeslotenTag ("H" ++ [(chr (n + 48))]) [] [Tekst string]


font :: [Attribuut] -> [HTML] -> HTML
font attributen htmls = GeslotenTag "FONT" attributen htmls


text :: String -> HTML
text string = Tekst string


document :: String -> [HTML] -> HTML
document string htmls = GeslotenTag "HTML" [] [GeslotenTag "HEAD" [] [GeslotenTag "TITLE" [] [Tekst string]]
                                              ,GeslotenTag "BODY" [] htmls
                                              ]

--(map li) htmls should be: (map li htmls)
-- 211,51-64
