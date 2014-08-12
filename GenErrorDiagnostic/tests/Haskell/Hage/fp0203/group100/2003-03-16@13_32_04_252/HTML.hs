module HTML where


data HTML = Tekst String
          | EnkelTag Tag [Attribuut]
          | DubbelTag Tag [Attribuut] [HTML]

type Tag = String


type Attribuut = (Naam, Waarde)

type Naam = String
type Waarde = String






type Requirement = (String, String)
test (DubbelTag "LI" _ (x:xs)) | (vergelijk x False) || (test DubbelTag "LI" [] xs) =  False






vergelijk (DubbelTag "UL" _ _) False = True

-- () is missing for "DubbelTag "LI" [] xs"
-- 22,58-82
