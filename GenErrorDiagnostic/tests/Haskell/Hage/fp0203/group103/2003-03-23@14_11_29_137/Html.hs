import Data.Maybe
import Pretty

eqString      :: String -> String -> Bool 
eqString = undefined
chr = undefined
ord = undefined
data HTML = Text String
          | TagSimp String [Attrib]
          | TagComp String [Attrib] [HTML]
type Attrib = (String, String)
voorbeeld :: HTML
voorbeeld = TagComp "UL" [] [
              (TagComp "LI" [] [(TagComp "FONT" [("COLOR","#0000FF"),("SIZE","5")] [(Text "Eerste")]), (Text " punt")]),
              (TagSimp "HR" []),
              (TagComp "LI" [] [(Text "Tweede punt")] )]

type Requirement = (String, String)


validate :: [Requirement] -> HTML -> Maybe Requirement
validate []         _    = Nothing
validate (req:reqs) html | not (validateH req "" html) = Just req
                         | otherwise                   = validate reqs html


validateH :: Requirement -> String -> HTML -> Bool
validateH     _                _    (Text _         )     = True
validateH     (rparT, rchildT) parT (TagSimp tag _  )     | eqString rchildT tag && not (eqString rparT parT) = False
                                                          | otherwise                                         = True
validateH req@(rparT, rchildT) parT (TagComp tag _ htmls) | eqString rchildT tag && not (eqString rparT parT) = False
                                                          | otherwise      = all (validateH req tag) htmls




ppHTML :: HTML -> Doc
ppHTML (Text txt)                = tekst txt
ppHTML (TagSimp tag attrs)       = openTag tag attrs
ppHTML (TagComp tag attrs htmls) | simple htmls = openTag tag attrs <|> foldr (<|>) leeg (map ppHTML htmls) <|> tekst("</" ++ tag ++ ">")
                                 | otherwise    = openTag tag attrs <-> springIn 1 (concatMap ppHTML htmls) <-> tekst("</" ++ tag ++ ">")


simple :: [HTML] -> Bool
simple htmls = foldr ((&&).partOfSimple) True htmls
     where partOfSimple (TagComp _ _ hts) = length hts == 1
           partOfSimple _                 = True


openTag :: String -> [Attrib] -> Doc
openTag tag attrs = tekst ("<" ++ tag ++ concatMap attView attrs ++ ">")


attView :: (String, String) -> String
attView (name, value) = " " ++ name ++ "=" ++ "\"" ++ value ++ "\""




color :: Int -> Int -> Int -> String
color r g b = "#" ++ twoDigits (hexadeca r) ++ twoDigits (hexadeca g) ++ twoDigits (hexadeca b)
     where twoDigits strValue = reverse $ take 2 (reverse strValue ++ repeat '0')



hexadeca :: Int -> String
hexadeca value | value < 10    = [chr (ord '0' + value)]
               | value < 16    = [chr (ord 'A' + value - 10)]
               | otherwise     = hexadeca (value / 16) ++ hexadeca (value `rem` 16)


colorTable :: [[(Int,Int,Int)]] -> HTML
colorTable css = TagComp "TABLE" [("HEIGHT", "400"), ("WIDTH", "400")] (makeRows css)
     where makeRows :: [[(Int, Int, Int)]] -> [HTML]
           makeRows  = map (\cs -> (TagComp "TR" [] (makeCells cs)))
           makeCells :: [(Int, Int, Int)] -> [HTML]
           makeCells = map (\c ->  (TagComp "TD" [("BGCOLOR", (uncurry3 color) c)] []))


uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a, b, c) = f a b c


ul :: [[HTML]] -> HTML
ul htmlss   = TagComp "UL" [] (map (TagComp "LI" []) htmlss)


h :: Int -> String -> HTML
h size str      = TagComp ("H" ++ (hexadeca size)) [] [Text str]



font :: [Attrib] -> [HTML] -> HTML
font atrs htmls = TagComp "FONT" atrs htmls


text :: String -> HTML
text str        = Text str


document :: String -> [HTML] -> HTML
document title body = TagComp "HTML" [] [
                         (TagComp "HEAD" [] [TagComp "TITLE" [] [Text title]]),
                         (TagComp "BODY" [] body)]




kleurenTabel :: HTML
kleurenTabel = document "Kleurentabel" [h 1 "Kleurentabel",
                                        ul [] listItems   ,
                                        colorTable kleuren]
      where listItems = [[font [("COLOR","#FF0000")] [text "Rood"] , text " loopt van boven naar onder van 0 tot en met 250 in stappen van 25"],
                         [font [("COLOR","#00FF00")] [text "Groen"], text " loopt van links naar rechts van 0 tot en met 250 in stappen van 25"],
                         [font [("COLOR","#0000FF")] [text "Blauw"], text " is overal 0"]]
            kleuren   = [[(x,y,0) | y <- [0, 25..250] ] | x <- [0, 25..250] ]


main :: IO ()
main = layout (ppHTML kleurenTabel)

-- ul [] listItems  should be ul listItems
-- 111,44-55
