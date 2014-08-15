module JeffHTML where
data HTML = Ptxt String
          | Stag (String, [Attrib])
          | Dtag (String, [Attrib]) [HTML] String

data Attrib = String := String

type Requirement = (String, String)

voorbeeld :: HTML
voorbeeld = Dtag ("HTML", [])
               [(Stag ("LI", [])),
                (Dtag ("UL", [])
                  [(Ptxt "hiephhoi")]
                 "/UL")]
            "/HTML"
eqString      :: String -> String -> Bool 
eqString = undefined
chr :: Int -> Char
chr = undefined
ord :: Char -> Int
ord = undefined
showInt       :: Int -> String
showInt = undefined

validate   :: [Requirement] -> HTML -> Maybe Requirement
validate [] _                                 = Nothing
validate ((a,b):xs) html1@(Dtag (_,_) html _) | check a b html html1 = validate xs html1
                                              | otherwise = Just (a,b)

check :: String -> String -> [HTML] -> HTML -> Bool
check _ _ [] _ = True
check a b (x:xs) html = (innercheck a b x html) && (check a b xs html)

innercheck :: String -> String -> HTML -> HTML -> Bool
innercheck a b (Stag (tag, _)) html | eqString b tag = outercheck a html
                                    | otherwise = True
innercheck _ _ _ _ = True

outercheck :: String -> HTML -> Bool
outercheck a (Dtag (tag, _) _ _) | eqString a tag = True
                                 | otherwise = False


color :: Int -> Int -> Int -> String
color r g b = '#' : map hexa [r1,r2,g1,g2,b1,b2]
              where r1 = r/16
                    r2 = mod r 16
                    g1 = g/16
                    g2 = mod g 16
                    b1 = b/16
                    b2 = mod b 16
                    hexa a | a <= 9  = chr (a + ord '0')
                           | a == 10 = 'A'
                           | a == 11 = 'B'
                           | a == 12 = 'C'
                           | a == 13 = 'D'
                           | a == 14 = 'E'
                           | a == 15 = 'F'


colorTable :: [[(Int, Int, Int)]] -> HTML
colorTable tab = Dtag ("TABLE", ["width" := "400", "height" := "400"]) (colorTab tab) "/TABLE"


colorTab :: [[(Int, Int, Int)]] -> [HTML]
colorTab [] = []
colorTab (x:xs) = (Dtag ("TR", []) (colorRow x) "/TR") : colorTab xs


colorRow :: [(Int, Int, Int)] -> [HTML]
colorRow [] = []
colorRow ((r,g,b):xs) = (Dtag ("TD", ["bgcolor" := color r g b]) [Ptxt ""] "/TD") : colorRow xs





h :: Int -> String -> HTML
h grootte tekst = Dtag (hx, []) [Ptxt tekst] chx
                  where hx = 'H' : showInt grootte
                        chx = '/' : 'H' : showInt grootte


font :: [Attrib] -> [HTML] -> HTML
font atts html = Dtag ("FONT", atts) html "/FONT"


text :: String -> HTML
text t = Ptxt t


document :: String -> [HTML] -> HTML
document hoofd doc = Dtag ("HTML", [])
                        [(Dtag ("HEAD", [])
                            [Dtag ("TITLE", []) hoofd "/TITLE"]
                         "/HEAD"),
                         (Dtag ("BODY", []) doc "/BODY")]
                     "/HTML"


kleurenTabel :: HTML
kleurenTabel = document "Kleurentabel"
               [h 1 "Kleurentabel",



                colorTable
                [[(0,0,0), (0,25,0), (0,50,0), (0,75,0), (0,100,0), (0,125,0), (0,150,0), (0,175,0), (0,200, 0), (0,225, 0), (0,250, 0)],
                 [(25,0,0), (25,25,0), (25,50,0), (25,75,0), (25,100,0), (25,125,0), (25,150,0), (25,175,0), (25,200, 0), (25,225, 0), (25,250, 0)],
                 [(50,0,0), (50,25,0), (50,50,0), (50,75,0), (50,100,0), (50,125,0), (50,150,0), (50,175,0), (50,200, 0), (50,225, 0), (50,250, 0)],
                 [(75,0,0), (75,25,0), (75,50,0), (75,75,0), (75,100,0), (75,125,0), (75,150,0), (75,175,0), (75,200, 0), (75,225, 0), (75,250, 0)],
                 [(100,0,0), (100,25,0), (100,50,0), (100,75,0), (100,100,0), (100,125,0), (100,150,0), (100,175,0), (100,200, 0), (100,225, 0), (100,250, 0)],
                 [(125,0,0), (125,25,0), (125,50,0), (125,75,0), (125,100,0), (125,125,0), (125,150,0), (125,175,0), (125,200, 0), (125,225, 0), (125,250, 0)],
                 [(150,0,0), (150,25,0), (150,50,0), (150,75,0), (150,100,0), (150,125,0), (150,150,0), (150,175,0), (150,200, 0), (150,225, 0), (150,250, 0)],
                 [(175,0,0), (175,25,0), (175,50,0), (175,75,0), (175,100,0), (175,125,0), (175,150,0), (175,175,0), (175,200, 0), (175,225, 0), (175,250, 0)],
                 [(200,0,0), (200,25,0), (200,50,0), (200,75,0), (200,100,0), (200,125,0), (200,150,0), (200,175,0), (200,200, 0), (200,225, 0), (200,250, 0)],
                 [(225,0,0), (225,25,0), (225,50,0), (225,75,0), (225,100,0), (225,125,0), (225,150,0), (225,175,0), (225,200, 0), (225,225, 0), (225,250, 0)],
                 [(250,0,0), (250,25,0), (250,50,0), (250,75,0), (250,100,0), (250,125,0), (250,150,0), (250,175,0), (250,200, 0), (250,225, 0), (250,250, 0)]]]

-- hoofd should be [Ptxt hoofd]
-- 96,49-53
