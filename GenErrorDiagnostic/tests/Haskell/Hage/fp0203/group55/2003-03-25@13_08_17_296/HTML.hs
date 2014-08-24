module HTML (module Pretty, module HTML) where
import Data.List
import Pretty

showInt       :: Int -> String
showInt = undefined


data HTML
  = Tekst String
  | EnkelTag TagNaam [Att]
  | DubbelTag TagNaam [Att] [HTML]

type Att         = (Naam, Waarde)
type TagNaam     = String
type Naam        = String
type Waarde      = String
type Requirement = (String, String)

testhtml :: HTML
testhtml =
 DubbelTag "UL" []
 [  DubbelTag "LI" [] [DubbelTag "FONT" [("COLOR", "#0000FF")] [Tekst "Eerste"], Tekst " punt"],
    EnkelTag "HR" [],
    DubbelTag "LI" [] [Tekst "Tweede punt"]
 ]

testhtml2 :: HTML
testhtml2 =
 DubbelTag "UL" []
 [  DubbelTag "LI" [] [DubbelTag "B" [] [Tekst "Eerste"], Tekst " punt"],
    DubbelTag "LI" [] [Tekst "Tweede punt"]
 ]








































ppHTML :: HTML -> Doc
ppHTML (Tekst string)               = tekst string
ppHTML (EnkelTag tagnaam att)       = tekst ("<" ++ tagnaam)
                                      <|> printAtt att
                                      <|> tekst ">"
ppHTML (DubbelTag tagnaam att [])   = tekst ("<" ++ tagnaam)
                                      <|> printAtt att
                                      <|> tekst ">"
                                      <|> tekst ("<" ++ "//" ++ tagnaam ++ ">")
ppHTML (DubbelTag tagnaam att html) = if  and (map simpelHTML html) then  tekst ("<" ++ tagnaam)
                                                     <|> printAtt att
                                                     <|> tekst ">"
                                                     <|> proc html
                                                     <|> tekst ("<" ++ "/" ++ tagnaam ++ ">")
                                                         else layoutHTML (DubbelTag tagnaam att html)



layoutHTML :: HTML -> Doc
layoutHTML (DubbelTag tagnaam att html)= tekst ("<" ++ tagnaam)
                                         <|> printAtt att
                                         <|> tekst ">"
                                         <-> springIn 1 (layoutHulp html)
                                         <-> tekst ("<" ++ "/" ++ tagnaam ++ ">")

layoutHulp :: [HTML] -> Doc
layoutHulp [] = []
layoutHulp (x:xs) = ppHTML x <-> layoutHulp xs


simpelHTML :: HTML -> Bool
simpelHTML (Tekst _)                                                      = True
simpelHTML (EnkelTag _ _)                                                 = True
simpelHTML (DubbelTag _ _ [html]) | length [html] == 1 && simpelHTML html = True
                                  | otherwise                             = False
simpelHTML (DubbelTag _ _ _)                                              = False












proc :: [HTML] -> Doc
proc [] = leeg
proc (x:xs) = ppHTML x <|> proc xs

printAtt :: [(Naam, Waarde)] -> Doc
printAtt [] = leeg
printAtt ((n,w) : rest) = tekst (" " ++ n ++ "=" ++ "\"" ++ w ++ "\"") <|> printAtt rest

hulp :: HTML -> IO ()
hulp html = layout (ppHTML html)

















ul :: [[HTML]] -> HTML
ul html = DubbelTag "UL" [] (ulLI html)

ulLI :: [[HTML]] -> [HTML]
ulLI (li:lis) = DubbelTag "LI" [] li : DubbelTag "LI" [] lis

h :: Int -> String -> HTML
h grootte kopje = DubbelTag ("H" ++ showInt grootte) [] [Tekst kopje]

font :: [Att] -> [HTML] -> HTML
font att html = DubbelTag "FONT" att html

text :: String -> HTML
text bla = Tekst bla

document :: String -> [HTML] -> HTML
document titel body = DubbelTag "HTML" [] [(DubbelTag "HEAD" [] [DubbelTag "TITLE" [] [Tekst titel]])
                                          ,(DubbelTag "BODY" [] body)]

-- DubbelTag "LI" [] lis should be: ulLI lis
-- 153,40-60
