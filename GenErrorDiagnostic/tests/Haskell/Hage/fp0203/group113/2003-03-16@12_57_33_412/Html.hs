module Html where

data HTML = Text String
          | EnkelTag Naam [Attribuut]
          | DubbelTag Naam [Attribuut] [HTML]

type Naam = String

data Attribuut = Attribuut (String,String)

type Requirement = (String, String)

test = DubbelTag "ul" [] [DubbelTag "li" [] [DubbelTag "font" [("color", "red")] [Text "Hoi \n boe"] ]]

-- should be Attribuut (...)
-- 13,64-79   13,63-80
