module HTML(module HTML, module Pretty) where

import Pretty
eqString      :: String -> String -> Bool 
eqString = undefined

data HTML
  = Text String
  | SingleTag String [String]
  | DoubleTag String [String] [HTML]


type Requirement = (String, String)
main :: IO ()
main = layout $ ppHTML vb
main2 :: IO ()
main2 = layout $ ppHTML vb2




vb :: HTML
vb = DoubleTag "HTML" []
        [ DoubleTag "HEAD" []
           [ DoubleTag "TITLE" [] [Text "Test"]
           ]
        , DoubleTag "BODY" ["onLoad=Bla"]
           [ DoubleTag "H1" [] [Text "Test"]
           , SingleTag "BR" []
           , SingleTag "BR" []
           , SingleTag "HR" ["align=center"]
           , DoubleTag "UL" ["number=1"]
              [ DoubleTag "LI" []
                 [ Text "Blaat"]
              , DoubleTag "LI" []
                 [ Text "Bler"]
              ]
           ]
        ]






vb2 :: HTML
vb2 = DoubleTag "EM" []
       [ DoubleTag "B" []
          [ Text "test" ]
       , Text "je"
       ]



vb3 :: HTML
vb3 = DoubleTag "UL" ["number=1"]
              [ DoubleTag "B" []
                 [ Text "Boe" ]
              , DoubleTag "LI" []
                 [ Text "Blaat"]
              , DoubleTag "LI" []
                 [ Text "Bler"]
              ]

vb4 :: HTML
vb4 = DoubleTag "LI" []
                 [ Text "Blaat"]





ppHTML :: HTML -> Doc
ppHTML (Text text)         = tekst text
ppHTML (SingleTag naam arg)
      | null arg           = tekst $ "<" ++ naam ++ ">"
      | otherwise          = tekst ("<" ++ naam) <+> tekst (unwords arg ++ ">")
ppHTML (DoubleTag naam arg nest)
      | (length nest == 1) = beginTag <|> nestedTags <|>  sluitTag

      | otherwise          = beginTag <-> nestedTags <-> sluitTag
            where beginTag         | null arg         = tekst $ "<" ++ naam ++ ">"
                                   | otherwise        = tekst $ "<" ++ naam ++ " " ++ unwords arg ++ ">"
                  nestedTags       | null nest        = leeg
                                   | length nest == 1 = concatMap ppHTML nest
                                   | otherwise        = springIn 2 (verticaleLijst (map ppHTML nest))

                  sluitTag                      = tekst $ "</"++naam++">"

















voorwaarden :: [Requirement]
voorwaarden = [ ("UL","LI")
              , ("HTML","BODY")
              , ("HTML","HEAD")
              , ("BODY","TABLE")
              , ("BODY","H")
              , ("BODY","HR")
              ]




validateReq :: HTML -> Requirement -> Maybe Requirement
validateReq (DoubleTag naam _ ((DoubleTag cnaam _ _):xs)) (a,b) | naam `eqString` a && (not.eqString) cnaam b = Just (a,b)
                                                                | otherwise = Nothing

-- should be: eqString
-- 119,89-91
