module HTML(module  Pretty, module HTML) where

import  Pretty
eqString      :: String -> String -> Bool 
eqString = undefined
showInt       :: Int -> String
showInt = undefined

data HTML = Tekst String
          | SingelTag String [Attribuut]
          | Tag String [Attribuut] [HTML]

type Attribuut = (Name,Value)
type Name = String
type Value= String

maakAtt:: [Attribuut]->String
maakAtt [] = ""
maakAtt ((x,y):ts)= " "++x++"="++"\""++y++"\""++ maakAtt ts

maakTag:: HTML->String
maakTag (Tekst txt)          = txt
maakTag (SingelTag tag atts)   = "<"++tag ++(maakAtt atts)++ "> "
maakTag (Tag tag atts htmls) =  "<"++tag ++(maakAtt atts)++ "> "
                                          ++  concat (map maakTag htmls)
                                          ++  "</"++tag++">"

type Requirement = Attribuut

validate :: [Requirement]->HTML->Maybe Requirement
validate (ts) html |null (concat(map ( `validatie` html) ts))= Nothing
                   |otherwise   = head (concat(map ( `validatie` html) ts))

validatie :: Requirement->HTML->[Maybe Requirement]
validatie (x,y) html =  if ( positie x (valid html) < positie y (valid html) )
                       then []
                       else [Just (x,y)]

valid :: HTML->[String]
valid  (Tag tag _ (x:xs))  = ((tag : (valid x))) ++ (concatMap valid xs)
valid  (Tag tag _ []) = [tag]
valid  (SingelTag _ _)    = []
valid (Tekst _ )          = []


positie::String->[String]->Int
positie _ [] = -1
positie p (x:xs) | eqString p x =  0
                 | otherwise    = 1 + positie p xs




openTag :: String->[Requirement]->String
openTag tag atts =  "<"++tag ++(maakAtt atts)++ "> "

sluitTag :: String->String
sluitTag tag = "</"++tag++">"

pHTML :: HTML->Doc
pHTML (Tekst txt) = [txt]
pHTML (SingelTag tag atts)= [openTag tag atts]
pHTML (Tag tag atts [Tekst txt])  =  [(openTag tag atts)] <|> [txt] <|>[ sluitTag tag]
pHTML (Tag tag atts [SingelTag tag1 atts1])  =  [(openTag tag atts)] <|> [openTag tag1 atts1] <|>[ sluitTag tag]
pHTML (Tag tag atts [])      = [(openTag tag atts)] <|> [ sluitTag tag]

pHTML (Tag tag atts [Tag tag2 att2 deels])   |simpel (Tag tag2 att2 deels)= [(openTag tag atts)] <|>
                                                          ( concatMap pHTML  deels) <|>
                                                          [ sluitTag tag]
                                             |otherwise =   [(openTag tag atts)]<->
                                                         ([" "] <|>(concatMap pHTML  deels))
                                                          <->  [ sluitTag tag]







ppHTML :: HTML ->Doc
ppHTML (Tag tag atts htms) =[(openTag tag atts)] <|> (concatMap pHTML htms) <|>[ sluitTag tag]
ppHTML (SingelTag tag atts)= [openTag tag atts]
ppHTML (Tekst txt) = [txt]






























color :: Int->Int->Int->String
color r g b | r<256 && g<256 && b<256 = "#"++ toString r ++ toString g ++ toString b
            |otherwise = "Al getallen moeten kleiner zijn dan 256"

toString :: Int->String
toString a = ["0123456789ABCDEF" !! (a/16)] ++ ["0123456789ABCDEF" !! (a `rem`16)]

colorTable :: [[(Int, Int, Int)]] -> HTML
colorTable colors                         = Tag "TABLE" [("WIDTH","400"), ("HEIGHT","400")](trs colors)

tds   :: [(Int,Int,Int)] -> [HTML]
tds []                          = []
tds((r,g,b):xs)                = [Tag "TD" [("BGCOLOR", (color r g b))] []]++(tds xs)

trs   :: [[(Int,Int,Int)]] -> [HTML]
trs []                          = []
trs (x:xs)                      = [Tag "TR" [] (tds x)] ++ (trs xs)

ul :: [[HTML]] -> HTML
ul (xss)     = Tag "UL" [](map li xss)

li :: [HTML]->HTML
li xs = Tag "LI" []xs

h :: Int->String->HTML
h n txt = Tag ("H"++showInt n)[][Tekst txt]

font :: [Attribuut] -> [HTML] -> HTML
font att html = Tag "Font" att html

text :: String->HTML
text txt = Tekst txt

document :: String->[HTML]->HTML
document titel html = Tag "HTML"[][Tag "Head"[][Tag "Title" [][Tekst titel]],Tag "Body"[]html]

kleurenTabel :: HTML
kleurenTabel = document "KleurenTabel" [(h 1 "KleurenTabel"),(ul test),(colorTable (kleuren 0))]

kleuren :: Int->[[(Int, Int, Int)]]
kleuren n |n<255= ( mixKleuren n  [0,25..255] ) : kleuren (n + 25)
          |otherwise = []

mixKleuren :: Int->[Int]->[(Int, Int, Int)]
mixKleuren n ns= [ (n,x,0) | x<-ns ]

main :: IO()
main =  (layout.lay) kleurenTabel


lay :: HTML->[String]
lay (Tekst txt) = [" " ++ txt]
lay (SingelTag aa att)= [openTag aa att]
lay (Tag aa att []) = [" "++(openTag aa att)++(sluitTag aa)]


lay (Tag aa att ( htmss)) |simpel (Tag aa att htmss)=     [(openTag aa att)++concat(concatMap lay htmss)++(sluitTag aa)]

                                            |otherwise =  [(openTag aa att)]<->
                                                          ([" "]<|>(concatMap lay htmss)) <-> [(sluitTag aa)]



simp :: HTML->Bool
simp (Tag _ _ [])=True
simp (Tag _ _ ((SingelTag _ _):_))= True
simp (Tag _ _ ((Tekst _):_))  = True
simp (Tag _ _ a) = length a == 1

simpel :: HTML->Bool
simpel (SingelTag _ _)=True
simpel (Tekst _)      = True
simpel (Tag _ _ lijst )= length lijst ==1 && all (map simpel lijst)




test= [           [Tag "Font"[("Color","#FF0000")][Tekst "Rood"],
                 Tekst "loopt van boven naar onder van 0 tot en met 250 in stappen van 25 "],
               [Tag "Font"[("Color","#00FF00")][Tekst "Groen"],
                 Tekst "loopt van links naar rechts van 0 tot en met 250 in stappen van 25 "],
               [Tag "Font"[("Color","#0000FF")][Tekst "Blauw"],
                 Tekst "is overal 0 "]]

-- all should be: and
-- 186,46-67
