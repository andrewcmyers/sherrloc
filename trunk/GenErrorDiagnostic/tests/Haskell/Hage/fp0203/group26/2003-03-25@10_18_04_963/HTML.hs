module HTML (module Pretty, module HTML) where

import Pretty
eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined
eqMaybe :: (a -> a -> Bool) -> (Maybe a) -> (Maybe a) -> Bool
eqMaybe = undefined

data HTML
  = Text String
  | ST String [Attrib]
  | DT String [Attrib] [HTML]

type Attrib = (String, String)

type Requirement = (String, String)

type TagTuple = (String, String)

main :: IO ()
main = layout (ppHTML kleurenTabel)


hex :: [String]
hex = ["0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F"]


example :: HTML
example =
  DT "HTML" []
  [ DT "HEAD" []
    [
    ]
  , DT "BODY" []
    [ DT "UL" []
      [ DT "LI" []
        [ DT "FONT" [("COLOR","#0000FF"),("COLOR","#oooof")]
          [ Text "Eerste"
          ]
        , Text " punt"
        ]
      , ST "HR" [("COLOR","#oooof"),("COLOR","#oooof")]
      , DT "LI" []
        [ Text "Tweede punt"
        ]
      ]
    ]
  ]

example2 :: HTML
example2 =
  DT "HTML" []
  [ DT "HEAD" []
    [
    ]
  , DT "BODY" []
    [
    ]
  ]

example3 :: HTML
example3 =
  DT "HTML" []
  [ DT "HEAD" []
    [
    ]
  ]

example4 :: HTML
example4 = ST "HR" [("Width","50")]

example5 :: HTML
example5 = DT "FONT" [("COLOR","#0000FF")] [(Text "eerste"), (Text "tweede")]

example6 :: HTML
example6 = DT "FONT" [] []
example7 ::HTML
example7 = DT "UL" [] [DT "LI" [] [(DT "B" [] [Text "1"]),(Text "12")],DT "LI" [] [Text "3"]]

example8 :: HTML
example8 = DT "TR" [] [(DT "TD" [] []),(DT "TD" [] [])]

example9 :: HTML
example9 =  DT "TR" [] [(DT "TD" [] [(DT "TD" [] []),(DT "TD" [] [])])]

example10 :: HTML
example10 = ST "HR" []





validate :: [Requirement] -> HTML -> Maybe Requirement
validate [] _ = Nothing
validate (req1:reqs) html | (eqMaybe nonCompliant Nothing failedReq) = validate reqs html
                          | otherwise = failedReq
                            where failedReq = validateReq req1 html


validateReq :: Requirement -> HTML -> Maybe Requirement
validateReq _ (Text _) = Nothing
validateReq _ (ST _ _) = Nothing
validateReq _ (DT _ _ []) = Nothing
validateReq req (DT name _ html) | (eqMaybe nonCompliant Nothing failedReq) = firstJust(map (validateReq req) html)
                                 | otherwise = failedReq
                                   where failedReq = reqCheck req [ (a,b) | a <- [name], b <- (map tagName html) ]


tagName :: HTML -> String
tagName (Text _) = "Text"
tagName (ST tagname _) = tagname
tagName (DT tagname _ _) = tagname


reqCheck :: Requirement -> [TagTuple] -> Maybe Requirement
reqCheck _ [] = Nothing
reqCheck req (tagTuple1:tagTuples) | nonCompliant tagTuple1 req = Just req
                                   | otherwise = reqCheck req tagTuples


nonCompliant :: TagTuple -> Requirement -> Bool
nonCompliant tt req = if eqString (snd tt) (snd req)
                      then not(eqString (fst tt) (fst req))
                      else False


firstJust :: [Maybe Requirement] -> Maybe Requirement
firstJust [] = Nothing
firstJust (maybeReq1:maybeReqs) | eqMaybe nonCompliant maybeReq1 Nothing = firstJust maybeReqs
                                | otherwise = maybeReq1

























ppHTML :: HTML -> Doc
ppHTML (Text [])                               = leeg
ppHTML (Text string)                           = tekst string
ppHTML (ST string attrib)                      = ["<" ++ string]                          <|>
                                                 [unwords(concatMap attribLayout attrib)] <|>
                                                 [">"]
ppHTML (DT string attrib htmls) |simple        = openDT                                   <|>
                                                 [unwords(concatMap ppHTML htmls)]        <|>
                                                 closeDT
                                |otherwise     = openDT                                   <->
                                                 springIn 1 (concatMap ppHTML htmls)      <->
                                                 closeDT
                                 where simple  = eqBool (simpleCheck (DT string attrib htmls)) True
                                       openDT  = ["<" ++ string]                          <|>
                                                 [unwords(concatMap attribLayout attrib)] <|>
                                                 [">"]
                                       closeDT = ["</" ++ string ++ ">"]








simpleCheck :: HTML -> Bool
simpleCheck (Text _)       = True
simpleCheck (ST _ _)       = True
simpleCheck (DT _ _ htmls) |numberDTwithin > 1                          = False
                           |(numberDTwithin == 1)  &&  (simpleWithin)   = False
                           |otherwise                                   = True
                            where numberDTwithin    = length(filter (eqBool True) (map checkDT htmls))
                                  simpleWithin      = eqBool False (and(map simpleCheck htmls))



checkDT :: HTML -> Bool
checkDT (Text _)   = False
checkDT (ST _ _)   = False
checkDT (DT _ _ _) = True


attribLayout :: Attrib -> Doc
attribLayout (attrib1,attrib2) = [" " ++ attrib1 ++ "=" ++ attrib2]





color :: Int -> Int -> Int -> String
color r g b = "#" ++ colorCalc r ++ colorCalc g ++ colorCalc b
              where colorCalc x = hex !! (x / 16) ++ hex !! (mod x 16)


colorTable :: [[(Int, Int, Int)]] -> HTML
colorTable [] = DT "Table" [("WIDTH","400"),("HEIGHT","400")] [DT "TR" [] [DT "TD" [] [Text "No colors specified"]]]
colorTable rgbtable = DT "Table" [("WIDTH","400"),("HEIGHT","400")] (map trCreator rgbtable)


trCreator :: [(Int, Int, Int)] -> HTML
trCreator [] = DT "TR" [] [DT "TD" [] [Text "No colors specified"]]
trCreator rgblist = DT "TR" [] (map tdCreator rgblist)
                    where tdCreator (r, g, b) = DT "TD" [("BGCOLOR",color r g b)] []


ul :: [[HTML]] -> HTML
ul [] = DT "UL" [] [DT "LI" [] [Text "Empty List"]]
ul listOfLists = DT "UL" [] (map liCreator [Text "listOfLists"])
          where liCreator list = DT "LI" [] list


h :: Int -> String -> HTML
h num string = DT ("H" ++ hex !! num) [] [Text string]


font :: [Attrib] -> [HTML] -> HTML
font attribs htmls = DT "FONT" attribs htmls


text :: String -> HTML
text string = Text string



document :: String -> [HTML] -> HTML
document string htmls = DT "HTML" []
                        [ DT "HEAD" []
                          [ DT "TITLE" []
                            [ Text string
                            ]
                          ]
                        , DT "BODY" []
                          htmls
                        ]





kleurenTabel :: HTML
kleurenTabel = document "Kleurentabel"
               [ h 1 "Kleurentabel"
               , ul
                 [
                   [ font [("COLOR", color 255 0 0)]
                     [ text "Rood"
                     ]
                   , text " loopt van boven naar onder van 0 tot en met 250 in stappen van 25"
                   ]
                 , [ font [("COLOR", color 0 255 0)]
                     [ text "Groen"
                     ]
                   , text " loopt van links naar rechts van 0 tot en met 250 in stappen van 25"
                   ]
                 ,[ font [("COLOR", color 0 0 255)]
                     [ text "Blauw"
                     ]
                   , text " is overal 0"
                   ]
                 ]
               , colorTable rgmix
               ]




rgmix :: [[(Int, Int, Int)]]
rgmix = [ [(0,0,0),(0,25,0),(0,50,0),(0,75,0),(0,100,0),(0,125,0),(0,150,0)
          ,(0,175,0),(0,200,0),(0,225,0),(0,250,0)
          ]
        , [(25,0,0),(25,25,0),(25,50,0),(25,75,0),(25,100,0),(25,125,0)
          ,(25,150,0),(25,175,0),(25,200,0),(25,225,0),(25,250,0)
          ]
        , [(50,0,0),(50,25,0),(50,50,0),(50,75,0),(50,100,0),(50,125,0)
          ,(50,150,0),(50,175,0),(50,200,0),(50,225,0),(50,250,0)
          ]
        , [(75,0,0),(75,25,0),(75,50,0),(75,75,0),(75,100,0),(75,125,0)
          ,(75,150,0),(75,175,0),(75,200,0),(75,225,0),(75,250,0)
          ]
        , [(100,0,0),(100,25,0),(100,50,0),(100,75,0),(100,100,0),(100,125,0)
          ,(100,150,0),(100,175,0),(100,200,0),(100,225,0),(100,250,0)
          ]
        , [(125,0,0),(125,25,0),(125,50,0),(125,75,0),(125,100,0),(125,125,0)
          ,(125,150,0),(125,175,0),(125,200,0),(125,225,0),(125,250,0)
          ]
        , [(150,0,0),(150,25,0),(150,50,0),(150,75,0),(150,100,0),(150,125,0)
          ,(150,150,0),(150,175,0),(150,200,0),(150,225,0),(150,250,0)
          ]
        , [(175,0,0),(175,25,0),(175,50,0),(175,75,0),(175,100,0),(175,125,0)
          ,(175,150,0),(175,175,0),(175,200,0),(175,225,0),(175,250,0)
          ]
        , [(200,0,0),(200,25,0),(200,50,0),(200,75,0),(200,100,0),(200,125,0)
          ,(200,150,0),(200,175,0),(200,200,0),(200,225,0),(200,250,0)
          ]
        , [(225,0,0),(225,25,0),(225,50,0),(225,75,0),(225,100,0),(225,125,0)
          ,(225,150,0),(225,175,0),(225,200,0),(225,225,0),(225,250,0)
          ]
        , [(250,0,0),(250,25,0),(250,50,0),(250,75,0),(250,100,0),(250,125,0)
          ,(250,150,0),(250,175,0),(250,200,0),(250,225,0),(250,250,0)
          ]
        ]

-- [Text "listOfLists"] should be: listOfLists
-- 225,44-63
