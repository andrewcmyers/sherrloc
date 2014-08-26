module PracDrie (module PracDrie, module List, module Pretty) where

import Data.List
import Pretty

eqString      :: String -> String -> Bool 
eqString = undefined
chr :: Int -> Char
chr = undefined
ord :: Char -> Int
ord = undefined
showInt       :: Int -> String
showInt = undefined

data HTML = OnlyText String
          | OneTag String Attribute
          | TwoTag String Attribute [HTML]

type Requirement = (String, String)

type Attribute = [(String, String)]


voorbeeld::HTML
voorbeeld1::HTML
voorbeeld2::HTML
voorbeeld3::HTML

voorbeeld = OneTag "HEAD"[("size","10")]
voorbeeld1 = TwoTag "UL"[("Color","red")][TwoTag "LI"[("FONT", "ARIEL")][]]
voorbeeld2 = TwoTag "UL"[("Color","blue")][OnlyText "abc"]
voorbeeld3 = TwoTag "BODY"[("Color","red")][TwoTag "UL"[("FONT", "ARIEL")][TwoTag "LI"[("size", "50")][]]]





test :: String -> Requirement -> HTML -> Bool
test _ _ (OnlyText _) = True
test parent (x,y)(OneTag a _) = if y `eqString` a then x `eqString` parent  else False
test parent (x,y) (TwoTag a _ _)  =  if y `eqString` a then x `eqString` parent else False


validate :: [Requirement] -> HTML -> Maybe Requirement
validate reqs html = validate' "" reqs html


validate' :: String -> [Requirement] -> HTML -> Maybe Requirement
validate' _ [] _ = Nothing
validate' parent (req:xss) html | test parent req html = Just req
                                | otherwise = validate' parent xss html






ppHTML :: HTML -> Doc
ppHTML (OnlyText a) = tekst a
ppHTML (OneTag a attr) =  tekst ("<" ++ a) <|>tekst (concat (map ppAttributes attr)) <|> tekst ">"
ppHTML (TwoTag a attr htmls) | and (map simpleElement htmls) = ppHTML (OneTag a attr) <|> (pphtmls htmls) <|> tekst ("</" ++ a ++">")
                             | otherwise = tekst ("<" ++ a) <|>tekst (concat( map ppAttributes attr)) <|> tekst ">" <-> springIn 2 (pphtmls htmls)<-> tekst("</" ++ a ++">")


ppAttributes :: (String,String) -> String
ppAttributes  (x,y) = " "++ x ++ "=" ++ y ++ " "


pphtmls::[HTML]-> Doc
pphtmls htmls = verticaleLijst (map ppHTML htmls)


simpleElement :: HTML -> Bool
simpleElement (OnlyText _) = True
simpleElement (OneTag _ _) = True
simpleElement (TwoTag _ _ htmls) = all simpleElement htmls && length htmls==1






color:: Int -> Int -> Int -> String
color a b c =  "#" ++ convert a ++ convert b ++ convert c


convert:: Int -> String
convert b = (intAndString( b/16))++(intAndString (rem b 16))


intAndString::Int-> String
intAndString a = if a<10 then showInt a else [chr (a+(ord 'A'-10))]



colorTable::[[(Int,Int, Int)]] -> HTML
colorTable lijst = TwoTag "TABLE" [("WIDTH", "400"), ("HEIGHT", "400")] (concat [makeRows lijst])


makeCells:: [(Int, Int, Int)]-> [HTML]
makeCells [(x,y,z)] = [TwoTag "TD" [("BGCOLOR", color x y z)] []]
makeCells ((x,y,z):rest) = TwoTag "TD" [("BGCOLOR", color x y z)] []: makeCells rest


makeRows:: [[(Int, Int, Int)]] -> [HTML]
makeRows [list]= [TwoTag "TR" [] (concat [(makeCells list)])]
makeRows (list:rest) = (TwoTag "TR"  [] (concat[makeCells list])): makeRows rest



ul :: [[HTML]] -> HTML
ul lists = TwoTag "UL" [] (map (TwoTag "LI" [] ) lists)



h :: Int -> String -> HTML
h x str = TwoTag ("H" ++ showInt x) [] [OnlyText str]



font :: [Attribute] -> [HTML] -> HTML
font [[x]] [y] = TwoTag "FONT" x [y]
font (xs:attr) (html:htmls) = TwoTag "FONT" xs  [html,font attr htmls]



text :: String -> HTML
text txt = (OnlyText txt)



document :: String -> [HTML] -> HTML
document str htmls = (TwoTag "HTML" [] [(TwoTag "HEAD" [] [(TwoTag "TITLE" [] [OnlyText str] )]), (TwoTag "BODY" [] (concat [htmls]))])



kleurenTabel :: HTML
kleurenTabel = document "COLORS" [(h 4 "KLEURENTABEL"),(fontColor),(colorTable allTupels)]

ownLines:: [[HTML]]
ownLines =[[(OnlyText "rood is overal 0")], [(OnlyText "groen is overal 0")], [(OnlyText "blauw is overal 0")]]

fontColor:: HTML
fontColor = font [[("color",(color 45 52 90)),("color",(color 78 55 65)),("color",(color 45 6 36))]] (concat ownLines)

colorRed:: (Int,Int,Int) -> [(Int,Int,Int)]
colorRed (r,g,b) = take 11 ((r,g,b) : colorGreen (r+25,g,b))


colorGreen :: (Int,Int,Int) -> [(Int,Int,Int)]
colorGreen (r,g,b) = take 11((r,g,b) : colorGreen (r,g+25,b))


allTupels :: [[(Int,Int,Int)]]
allTupels = putTogether 11 [(x,y,0)|x<-[0,25..250], y<-[0,25..250]]


putTogether :: Int -> [a] -> [[a]]
putTogether _ [] = []
putTogether n xs = take n xs : putTogether n (drop n xs)


main :: IO ()
main = layout(ppHTML(kleurenTabel))

-- the parameter [[x]] should be: [x]
-- 122,6-10
