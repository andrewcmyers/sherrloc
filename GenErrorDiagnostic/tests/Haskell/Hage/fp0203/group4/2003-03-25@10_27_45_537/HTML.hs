module HTML(module HTML, module Pretty) where

import Pretty
chr :: Int -> Char
chr = undefined
ord :: Char -> Int
ord = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
toUpper :: Char -> Char
toUpper = undefined

data HTML
        = Text String
        | SingleTag String [Attribute]
        | DoubleTag String [Attribute] [HTML]

data Attribute = Attr String String

type Requirement = (String, String)




kleurenTabel                                   :: HTML
kleurenTabel                                   = document "Kleurentabel" [ h 1 "Kleurentabel"
                                                                         , ul [ [font [(Attr "color" (color 255 0 0))] [ text "Rood" ]
                                                                                , text " loopt van boven naar onder van 0 tot en met 250 in stappen van 25"
                                                                                ]
                                                                              , [font [(Attr "color" (color 0 255 0))] [ text "Groen" ]
                                                                                , text " loopt van links naar rechts van 0 tot en met 250 in stappen van 25"
                                                                                ]
                                                                              , [font [(Attr "color" (color 0 0 255))] [ text "Blauw" ]
                                                                                , text " is overal 0"
                                                                                ]
                                                                              ]
                                                                         , colorTable generateColors
                                                                         ]

main                                           :: IO ()
main                                           = layout $ ppHTML $ kleurenTabel






eqStringIC                                     :: String -> String -> Bool
eqStringIC stringL' stringR'                   = eqString stringL stringR
                                               where stringL = map toUpper stringL'
                                                     stringR = map toUpper stringR'

isSimple                                       :: HTML -> Bool
isSimple (DoubleTag _ _ tags)                  = (length tags == 1) && isSimple (head tags)
isSimple (SingleTag _ _)                       = True
isSimple (Text _)                              = True


listIsSimple                                   :: HTML -> Bool
listIsSimple (DoubleTag _ _ tags)              = all isSimple tags


ppOpeningTag                                   :: String -> [Attribute] -> Doc
ppOpeningTag name attr                         | not.null $ attr        = tekst ("<" ++ name) <+> ppAttributeList attr <|> tekst ">"
                                               | otherwise              = tekst $ "<" ++ name ++ ">"


ppCloseTag                                     :: String -> Doc
ppCloseTag name                                = tekst "</" <|> tekst (name ++ ">")


ppAttributeList                                :: [Attribute] -> Doc
ppAttributeList []                             = leeg
ppAttributeList [x]                            = ppAttribute x
ppAttributeList (x:xs)                         = ppAttribute x <+> ppAttributeList xs

ppAttribute                                    :: Attribute -> Doc
ppAttribute (Attr name value)                  = text $ name ++ "=\"" ++ value ++ "\""

decToHexNotation                               :: Int -> Char
decToHexNotation dec                           | dec < 0 || dec > 15 = error "decToHexNotation: parameter out of range"
                                               | otherwise =
                                                        case dec of
                                                         10 -> 'A'
                                                         11 -> 'B'
                                                         12 -> 'C'
                                                         13 -> 'D'
                                                         14 -> 'E'
                                                         15 -> 'F'
                                                         other -> chr (other + ord '0')


fst3                                           :: ( a, a, a) -> a
fst3 (x,_,_)                                   = x

snd3                                           :: ( a, a, a) -> a
snd3 (_,x,_)                                   = x

trd3                                           :: ( a, a, a) -> a
trd3 (_,_,x)                                   = x

generateColors                                 :: [ [(Int, Int, Int)] ]
generateColors                                 = map (\red -> [ (red, green, 0) | green <- colorList] ) colorList
                                               where colorList = [0, 25..250]





validate                                        :: [Requirement] -> HTML -> Maybe Requirement
validate reqs html                              = head $ validateAll reqs html

validateAll                                     :: [Requirement] -> HTML -> [Maybe Requirement]
validateAll reqs html                           | null notFullfilledReqs = [Nothing]
                                                | otherwise              = map Just notFullfilledReqs
                                                where notFullfilledReqs = filter (\req -> not $ isValid req html "") reqs

isValid                                         :: Requirement -> HTML -> String -> Bool
isValid req (DoubleTag name _ tags) parentName  | eqStringIC name (snd req) = eqStringIC parentName (fst req) && checkSubTree
                                                | otherwise                 = checkSubTree
                                                where checkSubTree = all (\html -> isValid req html name) tags
isValid req (SingleTag name _ ) parentName      | eqStringIC name (snd req) = eqStringIC parentName (fst req)
                                                | otherwise                 = True
isValid _ (Text _) _                            = True

ppHTML                                          :: HTML -> Doc
ppHTML dt@(DoubleTag name attr tags)            | listIsSimple dt       = openingTag <|> (foldl (<|>) [] (map ppHTML tags)) <|> closeTag
                                                | otherwise             = openingTag <-> springIn 1 (foldl (<->) [] (map ppHTML tags)) <-> closeTag
                                                where openingTag = ppOpeningTag name attr
                                                      closeTag   = ppCloseTag name
ppHTML (SingleTag name attr)                    = ppOpeningTag name attr
ppHTML (Text l_Text)                            = tekst l_Text

color                                           :: Int -> Int -> Int -> String
color r g b                                     | checkValues [r, g, b]         = "#" ++ concatMap convertValue [r, g, b]
                                                | otherwise                     = error "color : Parameters are not in the valid range"
                                                where checkValues  = all (\x -> x >= minInt && x <= maxInt)
                                                                where minInt = 0
                                                                      maxInt = 255
                                                      convertValue dec = decToHexNotation(dec/16) : decToHexNotation(dec `rem` 16) : []

colorTable                                      :: [[(Int, Int, Int)]] -> HTML
colorTable intTupelLists                        = DoubleTag "table" [ Attr "width" "400", Attr "height" "400"] (makeRows intTupelLists)
                                                where makeRows intTupelLists' = map (\list -> DoubleTag "tr" [] (makeCols list)) intTupelLists'
                                                      makeCols intTupels      = map (\intTupel -> DoubleTag "td" [ makeColor intTupel ] [] ) intTupels
                                                      makeColor intTupel      = Attr "bgColor" (color (fst3 intTupel) (snd3 intTupel) (trd3 intTupel))

ul                                              :: [[HTML]] -> HTML
ul listItems                                    = DoubleTag "ul" [] $ constructItems listItems
                                                where constructItems = map (DoubleTag "li" [])

h                                               :: Int -> String -> HTML
h num l_Text                                    | num > 6 || num < 1    = error "h: number parameter out of bounds"
                                                | otherwise             = DoubleTag hNum [] [ Text l_Text]
                                                where hNum = "h" ++ [chr (num + ord '0')]

font                                            :: [Attribute] -> [HTML] -> HTML
font attr tags                                  = DoubleTag "font" attr tags

text                                            :: String -> HTML
text                                            = Text

document                                        :: String -> [HTML] -> HTML
document title bodyTags                         = DoubleTag "html" [] [
                                                                       DoubleTag "head" [] [
                                                                                           DoubleTag "title" [] [ Text title ]
                                                                                           ]
                                                                      ,DoubleTag "body" [] bodyTags
                                                                      ]

-- text should be: tekst
-- 78,50-53
