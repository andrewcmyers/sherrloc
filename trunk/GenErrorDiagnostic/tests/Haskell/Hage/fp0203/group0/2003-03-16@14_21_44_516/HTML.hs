module HTML(module Pretty, module HTML) where
import GHC.Base
import Pretty

data HTML = Text String
          | SingleTag String [Attribuut]
          | DoubleTag String [Attribuut] [HTML]

type Attribuut = (String,String)

vb :: HTML
vb = DoubleTag "BODY" [] (DoubleTag "LI" [] (SingleTag "UL" [("color=234567", "sdfhgdh")]))



type Requirement = (String, String)

valideer :: HTML -> Requirement -> Maybe Requirement
valideer (Text _ ) _         = Nothing
valideer (SingleTag _ _ ) _  = Nothing
valideer (DoubleTag tag _ htmls) req | eqString tag (snd req) = Just req
                                     | eqString tag (fst req) = Nothing
                                     | otherwise              = valideer htmls req













validate :: [Requirement] -> HTML -> Maybe Requirement
validate reqs html = alleenEenJust (map (valideer html) reqs)

alleenEenJust :: [Maybe Requirement] -> Maybe Requirement
alleenEenJust (Nothing:reqs) = alleenEenJust reqs
alleenEenJust []             = Nothing
alleenEenJust (Just a:_)     = Just a

regels :: [Requirement]
regels = [("UL", "LI"), ("HTML", "BODY"), ("HTML", "HEAD"), ("BODY", "H"), ("BODY", "HR"), ("BODY", "TABLE")]



toon :: HTML -> String
toon (Text bla)                = bla
toon (SingleTag tag atts)      = "<" ++ tag ++ toonAtts atts ++ ">"
toon (DoubleTag tag atts html) = "<" ++ tag ++ toonAtts atts ++ ">" ++ concat (map toon html) ++ "</" ++ tag ++ ">"

toonAtts :: [Attribuut] -> String
toonAtts (att:atts) = (fst att) ++ " = " ++ (snd att) ++ toonAtts atts

-- [HTML] in definition should be HTML
-- 7,42-47
