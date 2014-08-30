module HTML2 (module HTML2, module Pretty) where

import Pretty

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqChar      :: Char -> Char -> Bool
eqChar = undefined
chr :: Int -> Char
chr = undefined
ord :: Char -> Int
ord = undefined
toUpper :: Char -> Char
toUpper = undefined

type Attribute = (String, String)

data HTML = Text String
          | Tag String [Attribute] [HTML]
          | EmptyTag String [Attribute]







test0 :: HTML
test0 = Tag "HTML" [] [ Tag "BODY" [("BGCOLOR", "#0099AA")
                                   , ("BACKGROUND", "Foo.gif")
                                   ]
                                   [ Text "Narf"
                                   , EmptyTag "HR" [("Size","2")
                                                   ,("WIDTH", "50%")
                                                   ]
                                   , Tag "UL" [] [ Tag "LI" [] [ Text "I-kunde suxx"]
                                                 , Tag "LI" [] [ Tag "FONT" [("COLOR", "#FF0000")] [Text "BSCW suxx too"] ]
                                                 , Tag "LI" [] [ Text "ullig"]
                                                 ]


                                   ]
                      ]

test1 :: HTML
test1 = Tag "HTML" [] [ Tag "BODY" [("BGCOLOR", "#0099AA")
                                   , ("BACKGROUND", "Foo.gif")
                                   ]
                                   [ Text "Narf"
                                   , EmptyTag "HR" [("Size","2")
                                                   ,("WIDTH", "50%")
                                                   ]
                                   , Tag "UL" [] [ Tag "LI" [] [ Text "I-kunde suxx"]
                                                 , Tag "LI" [] [ Tag "FONT" [("COLOR", "#FF0000")] [Text "BSCW suxx too"] ]
                                                 , Tag "LI" [] [ Text "ullig"]
                                                 ]

                                   , Tag "LI" [] [ EmptyTag "HR" [] ]
                                   ]
                      ]
test2 :: HTML
test2 = Tag "HTMl" [] [ Tag "BODy" [("BGCOLOr", "#0099AA")
                                   , ("BACKGROUNd", "Foo.gif")
                                   ]
                                   [ Text "Narf"
                                   , EmptyTag "Hr" [("Size","2")
                                                   ,("WIDTh", "50%")
                                                   ]
                                   , Tag "Ul" [] [ Tag "Li" [] [ Text "I-kunde suxx"]
                                                 , Tag "Li" [] [ Tag "FONt" [("COLOr", "#FF0000")] [Tag "b" [] [Text "BSCW suxx"], Text "too" ] ]
                                                 , Tag "Li" [] [ Text "ullig"]
                                                 ]
                                   , Tag "Ul" [] [ EmptyTag "Hr" [] ]

                                   ]
                      ]

test3 :: HTML
test3 =  Tag "UL" [] [ Tag "LI" [] [ Text "I-kunde suxx"]
                     , Tag "LI" [] [ Tag "FONT" [("COLOR", "#FF0000")] [Text "BSCW suxx too"] ]
                     , Tag "LI" [] [ Text "ullig"]
                     ]

test4 :: HTML
test4 =  Tag "BODy" [] [ EmptyTag "HR" [] ]

test5 :: HTML
test5 = Tag "HTMl" [] [ Tag "BODy" [("BGCOLOr", "#0099AA")
                                   , ("BACKGROUNd", "Foo.gif")
                                   ]
                                   [ Text "Narf"
                                   , EmptyTag "Hr" [("Size","2")
                                                   ,("WIDTh", "50%")
                                                   ]
                                   , Tag "Ul" [] [ Tag "Li" [] [ Text "I-kunde suxx"]
                                                 , Tag "Li" [] [ Tag "FONt" [("COLOr", "#FF0000")] [Text "BSCW suxx too"] ]
                                                 , Tag "Li" [] [ Text "ullig"]
                                                 ]


                                   , Tag "TABLE" [("Width","50")] [ Tag "TR" [] [ Tag "TD" [] [ Text "Foo " ]
                                                                                , Tag "TD" [] [ Text "Bar!" ]
                                                                                ]
                                                                  ]
                                   ]
                      ]

test6 :: HTML
test6 = Tag "EM" [] [Tag "B" [] [Text "tekst"], Text "je" ]

test7 :: IO ()
test7 = layout (ppHTML ( Tag "UL" [] [ Tag "LI" [] [Tag "FOnt" [] [ Text "  Foo  "], Text "  Bar  " ]
                                     , Tag "LI" [] [Tag "FOnt" [] [ Text "  Foo  "] ]
                                     ]
                )       )

test8 :: IO ()
test8 = layout (ppHTML ( Tag "UL" [] [ Tag "LI" [] [Tag "FOnt" [] [ Text "  Foo  "] ]
                                     , Tag "LI" [] [Tag "FOnt" [] [ Text "  Foo  "] ]
                                     ]
                )       )







type Requirement = (String, String)




























































validate :: [Requirement] -> HTML -> Maybe Requirement
validate reqs html@(Tag name _ children) = case length illegalChildren of
                                        0 -> validate reqs html
                                        _ -> Just (name, head illegalChildren)
                                      where illegalChildren :: [String]
                                            illegalChildren = concatMap isIllegalChild children
                                            isIllegalChild :: HTML -> [String]
                                            isIllegalChild (Tag child _ _) = if any (\ x -> not (eqString x name)) (map fst (filter (\ (_,b) -> eqString b child ) reqs)) then
                                                                                 child
                                                                             else
                                                                                 []









ppHTML :: HTML -> Doc
ppHTML (Text str) = tekst str
ppHTML (EmptyTag name attr) = tekst ( '<' : map toUpper name ++ showAttributes attr ++ ">" )
ppHTML (Tag name attr html) | simple html = tekst  openOpenTag          <|> afterSimpleOpenTag
                            | otherwise   = tekst (openOpenTag ++ ">" ) <-> afterNonSimpleOpenTag
                              where afterSimpleOpenTag    = tekst ( ">" ) <|> simpleTags html  <|> closeTag
                                    afterNonSimpleOpenTag = springIn 1 (concatMap ppHTML html) <-> closeTag

                                    closeTag              = tekst ( "</" ++ map toUpper name ++ ">" )
                                    openOpenTag           =           '<' : map toUpper name ++ showAttributes attr

simpleTags :: [HTML] -> Doc
simpleTags [] = leeg
simpleTags (x:xs) = ppHTML x <|> simpleTags xs

simple :: [HTML] -> Bool
simple [] = True
simple ((Text _): x ) = simple x
simple ((EmptyTag _ _) : x) = simple x
simple ((Tag _ _ x) : y) = length x == 1 && simple x && simple y

showAttributes :: [Attribute] -> String
showAttributes = concatMap (\ attr -> " " ++  map toUpper (fst attr)  ++ "=\""  ++ map toUpper (snd attr)  ++ "\"" )











color :: Int -> Int -> Int -> String
color r g b | r > 255 || g > 255 || b > 255 = error "Numbers to large for a color"
            | otherwise = "#" ++ hex r ++ hex g ++ hex b

hex :: Int -> String
hex x = toHex (x / 16) : toHex ( x `rem` 16) : []

toHex :: Int -> Char
toHex x | x > 9 = ['A', 'B', 'C', 'D', 'E', 'F'] !! (x - 10)
        | otherwise = digitChar x

digitChar :: Int -> Char
digitChar x = chr (x + ord '0')



type Color = (Int, Int, Int)

colorTable :: [[Color]] -> HTML
colorTable colorss = Tag "TABLE" [("WIDTH","400"),("HEIGHT","400")] (map colorTableTR colorss)

colorTableTR :: [Color] -> HTML
colorTableTR colors = Tag "TR" [] (map colorTableTD colors)

colorTableTD :: Color -> HTML
colorTableTD (r, g, b) = Tag "TD" [("BGCOLOR", color r g b)] []




ul :: [[HTML]] -> HTML
ul htmlss = Tag "UL" [] (map li htmlss)

li :: [HTML] -> HTML
li htmls = Tag "LI" [] htmls



h :: Int -> String -> HTML
h size str | size > 6 || size < 1 = error "Hx size error"
           | otherwise = Tag ('H' : digitChar size : []) [] [Text str]



font :: [Attribute] -> [HTML] -> HTML
font attrs htmls = Tag "FONT" attrs htmls



text :: String -> HTML
text str = Text str



document :: String -> [HTML] -> HTML
document title htmls = Tag "HTML" []
                       [ Tag "HEAD" []
                         [ Tag "TITLE" []
                           [ Text title ]
                         ]
                       , Tag "BODY" [] htmls
                       ]







main :: IO ()
main = layout (ppHTML kleurenTabel)

kleurenTabel :: HTML
kleurenTabel = document "Kleurentabel"
                 [ h 1 "Kleurentabel"
                   , ul [ [ font [("COLOR", "#FF0000")] [ text "Rood" ]
                          , text " loopt van boven naar onder van 0 tot en met 250 in stappen van 25"
                          ]
                        , [ font [("COLOR", "#00FF00")] [ text "Groen" ]
                          , text " loopt van boven naar onder van 0 tot en met 250 in stappen van 25"
                          ]
                        , [ font [("COLOR", "#0000FF")] [ text "Blauw" ]
                          , text " is overal 0"
                          ]
                        ]
                 , colorTable (getColors [0..10] [0..10])

                 ]



groepeer :: Int -> [a] -> [[a]]
groepeer _ [] = []
groepeer n xs = take n xs : groepeer n (drop n xs)

getColors :: [Int] -> [Int] -> [[Color]]
getColors [] _ = []
getColors (r:rs) gs = (map (\ g -> (25*r, 25*g, 0) ) gs) : getColors rs gs










sig::IO ();sig=layout(ppHTML sig');sig'::HTML;sig'=document blah[Tag"table"[("width","830"),("height","163"),("cellpadding","0")][r1,r2,r3,r4,r5,r6,r7]]
r1,r2,r3,r4,r5,r6,r7::HTML
r1=Tag"tr"[](replicate 36(Tag"TD"[("bgcolor",e 0)][]))
r2=Tag"tr"[](r1' 1++r2' 1++r4' 1++r3' 1++r4' 1++r1' 1++r1' 1)
r3=Tag"tr"[](r1' 2++r4' 2++r3' 2++r4' 2++r3' 2++r4' 2++r1' 2++td(e 2)++td(c 2)++td(c 2)++td(e 2)++td(c 2)++td(c 2)++td(e 2))
r4=Tag"tr"[](td(e 3)++(replicate 5(td'(c 3)))++td(e 3)++r5' 3++r4' 3++r3' 3++r4' 3++r1' 3++r4' 3++td(c 3)++r4' 3)
r5=Tag"tr"[](r1' 4++r4' 4++r3' 4++r4' 4++r3' 4++r4' 4++r1' 4++r1' 4)
r6=Tag"tr"[](r1' 5++r2' 5++r2' 5++r4' 5++td(e 5)++r5' 5++r1' 5)
r7=Tag"tr"[](replicate 36(Tag"TD"[("bgcolor",e 6)][]))
r1',r2',r3',r4',r5'::Int->[HTML]
r1' i=r4' i++td(e i)++r4' i
r2' i=td(e i)++(replicate 4(td'(c i)))++td(e i)
r3' i=replicate 3(td'(e i))
r4' i=td(e i)++td(c i)++td(e i)
r5' i=td(e i)++(replicate 3(td'(c i)))++td(e i)++td(e i)
e::Int->String;e i=color(132-10*i)(173-10*i)(190-10*i);c::Int->String;c i=color(240-10*i)(110-10*i)(110-10*i)
td::String->[HTML];td clr=[Tag"TD"[("bgcolor",clr)][]] ;td'::String->HTML;td' clr=Tag"TD"[("bgcolor",clr)][]
blah::[Char];blah=map chr(map(\x->x-1)(map ord(blah')))
blah'::String;blah' = "Bmm!Zpvs!Cbtf!Bsf!Cfmpoh!Up!Vt"









stringTable :: String -> Int -> IO ()
stringTable str int = layout (ppHTML (stringTable' (map toUpper str) int) )

stringTable' :: String -> Int -> HTML
stringTable' str height = document str
                             [ Tag "table" [ ("width",int2Str (strWidth * height))
                                           , ("height",int2Str (height * 9))
                                           , ("cellpadding", "0")
                                           , ("cellspacing", "1")
                                           ]
                             (emptyBar 0 ++ (tableRows str) ++ emptyBar 9 )
                             ]
                             where emptyBar i = [ Tag "TR"
                                                      []
                                                      (replicate
                                                           (strWidth)
                                                           (Tag "TD"
                                                              [("bgColor", charToColor ' ' i)]
                                                              []
                                                            )
                                                      )
                                                ]
                                   strWidth = stringWidth str

int2Str :: Int -> String
int2Str = map digitChar
          . reverse
          . map (`rem` 10)
          . takeWhile (/= 0)
          . iterate (/10)

tableRows :: String -> [HTML]
tableRows str = tableRows' ( map (\y -> map (\x -> " "++x++" ") (charToTableStrings y) ) str)

tableRows' :: [[String]] -> [HTML]
tableRows' strss = stringArrayInterpreter (map concat (transpose strss)) 1

transpose               :: [[a]] -> [[a]]
transpose []             = []
transpose ([] : xss)     = transpose xss
transpose ((x:xs) : xss) = (x : [r | (r:_) <- xss]) :
                           transpose (xs : [ t | (_:t) <- xss])

stringArrayInterpreter :: [String] -> Int -> [HTML]
stringArrayInterpreter [] _ = []
stringArrayInterpreter (str:strs) rowNo = Tag "TR" [] (rowInterpreter str rowNo) : stringArrayInterpreter strs (rowNo + 1)

rowInterpreter :: [Char] -> Int -> [HTML]
rowInterpreter [] _ = []
rowInterpreter (char:chars) rowNo = Tag "TD" [("BGCOLOR", charToColor char rowNo)] [] : rowInterpreter chars rowNo

charToColor :: Char -> Int -> String
charToColor clr i | clr `eqChar` ' ' = color (132-10*i) (173-10*i) (190-10*i)
                  | clr `eqChar` '1' = color (240-10*i) (110-10*i) (110-10*i)
                  | otherwise = "#00FF00"

stringWidth :: [Char] -> Int
stringWidth [] = 0
stringWidth (char:chars) | char `eqChar` 'i' = 3 + stringWidth chars
                         | char `eqChar` ' ' = 5 + stringWidth chars
                         | elemBy eqChar char width5 = 7 + stringWidth chars
                         | otherwise = 6 + stringWidth chars
                         where width5 = ['M', 'N', 'T', 'V', 'W', 'X', 'Y', 'Z']

charToTableStrings :: Char -> [String]
charToTableStrings clr = case clr of
            'I' -> ["1", "1", "1", "1", "1", "1", "1"]
            ' ' -> ["   ", "   ", "   ", "   ", "   ", "   ", "   "]
            'A' -> [" 11 ", "1  1", "1  1", "1111", "1  1", "1  1", "1  1"]
            'B' -> ["111 ", "1  1", "1  1", "111 ", "1  1", "1  1", "111 "]
            'C' -> [" 11 ", "1  1", "1   ", "1   ", "1   ", "1  1", " 11 "]
            'D' -> ["111 ", "1  1", "1  1", "1  1", "1  1", "1  1", "111 "]
            'E' -> ["1111", "1   ", "1   ", "111 ", "1   ", "1   ", "1111"]
            'F' -> ["1111", "1   ", "1   ", "111 ", "1   ", "1   ", "1   "]
            'G' -> [" 11 ", "1  1", "1   ", "1 11", "1  1", "1  1", " 11 "]
            'H' -> ["1  1", "1  1", "1  1", "1111", "1  1", "1  1", "1  1"]
            'J' -> ["1111", "   1", "   1", "   1", "   1", "1  1", " 11 "]
            'K' -> ["1  1", "1  1", "1 1 ", "11  ", "1 1 ", "1  1", "1  1"]
            'L' -> ["1   ", "1   ", "1   ", "1   ", "1   ", "1   ", "1111"]
            'O' -> [" 11 ", "1  1", "1  1", "1  1", "1  1", "1  1", " 11 "]
            'P' -> ["111 ", "1  1", "1  1", "111 ", "1   ", "1   ", "1   "]
            'Q' -> [" 11 ", "1  1", "1  1", "1  1", "1  1", "1 11", " 111"]
            'R' -> ["111 ", "1  1", "1  1", "111 ", "1 1 ", "1  1", "1  1"]
            'S' -> [" 11 ", "1  1", "1   ", " 11 ", "   1", "1  1", " 11 "]
            'U' -> ["1  1", "1  1", "1  1", "1  1", "1  1", "1  1", " 11 "]
            'M' -> ["1   1", "11 11", "1 1 1", "1 1 1", "1   1", "1   1", "1   1"]
            'N' -> ["1   1", "11  1", "11  1", "1 1 1", "1  11", "1  11", "1   1"]
            'T' -> ["11111", "  1  ", "  1  ", "  1  ", "  1  ", "  1  ", "  1  "]
            'V' -> ["1   1", "1   1", "1   1", "1   1", " 1 1 ", " 1 1 ", "  1  "]
            'W' -> ["1   1", "1   1", "1   1", "1   1", "1 1 1", "1 1 1", " 1 1 "]
            'X' -> ["1   1", "1   1", " 1 1 ", "  1  ", " 1 1 ", "1   1", "1   1"]
            'Y' -> ["1   1", "1   1", " 1 1 ", " 1 1 ", "  1  ", "  1  ", "  1  "]
            'Z' -> ["11111", "   1 ", "   1 ", "  1  ", " 1   ", " 1   ", "11111"]
            _   -> [" 11 ", "1  1", "   1", "  1 ", " 1  ", "    ", " 1  "]


-- child should be: [child]
-- 200,82-86
