module Pretty where

{- Horizontale compositie <|> bindt 
   sterker dan verticale <->.
   a <|> b <-> c <|> d wordt dus gelezen 
   als (a <|> b) <-> (c <|> d)
-}

infixl 4 <->
infixl 5 <|>

{- Het type Doc is geimplementeerd als een 
   lijst van strings. Iedere
   string representeert een regel in het document.
-}

type Doc = [String]

{- leeg geeft een leeg document terug. 
   Horizontale en verticale
   compositie van dit document met een ander
   geeft het andere document ongewijzigd terug.    
-}

leeg     :: Doc               -- leeg document

{- Met de functie "tekst" kan je 1 regel 
   tekst omvormen tot een document. In de String
   parameter mogen dus geen regel-overgangen (\n) zitten.
-}

tekst    :: String -> Doc     -- document met 1 regel tekst

{- Horizontale compositie zet twee documenten 
   naast elkaar. Het tweede document komt achter 
   de laatste regel van het eerste document. 
   Stel de documenten zijn:

     abcd
     defgh
 
   en
    
     ijklmn
     opqrs

   Dan is de horizontale compositie:

     abcd
     defghijklmn
          opqrs

-}

(<|>)    :: Doc -> Doc -> Doc

{- Verticale compositie zet twee documenten onder elkaar.
   Stel de documenten zijn:

     abcd
     defgh
 
   en
    
     ijklmn
     opqrs

   Dan is de verticale compositie:

     abcd
     defgh
     ijklmn
     opqrs

-}

(<->)    :: Doc -> Doc -> Doc

{- Met de functie springIn kan je een document 
   met een gegeven aantal spaties laten inspringen.
-}

springIn :: Int -> Doc -> Doc

{- De functie layout maakt het mogelijk het
   document op een mooie manier af te drukken 
   op het scherm. In Hugs:
   
   Prelude> layout ...

   Op de plek van de puntjes schrijf iets van type Doc
-}

layout   :: Doc -> IO ()      -- een document afdrukken 

{- De implementatie -}

-- leeg document
leeg = []

-- document met 1 regel tekst
tekst string
  = [ string ]

-- horizontale compositie
[] <|> rechts
  = rechts
links <|> []
  = links
links <|> rechts
  = init links 
    ++ 
    [ last links ++ head rechts ]
    ++
    springIn inspringing (tail rechts)
  where
    inspringing = length (last links)

-- vertical compositie
boven <-> onder
  = boven ++ onder

-- een document inspringen
springIn n regels
  = map spatiesErvoor regels
  where
    spatiesErvoor regel = spaties n ++ regel

-- een document afdrukken 
layout doc
  = putStr (unlines doc)

{- Hulpfuncties -}

spaties :: Int -> String
spaties n = replicate n ' '

infixl 4 <+>

(<+>) :: Doc -> Doc -> Doc
doc1 <+> doc2 = doc1 <|> springIn 1 doc2

kommaLijst :: [Doc] -> Doc
kommaLijst [] = leeg
kommaLijst [x] = x
kommaLijst (x:xs) = x <|> tekst "," <+> kommaLijst xs

haskellLijst :: [Doc] -> Doc
haskellLijst docs 
  = tekst "[" <|> kommaLijst docs <|> tekst "]"

utrechtseLijst :: [Doc] -> Doc
utrechtseLijst []
  = tekst "[]"
utrechtseLijst (doc:docs)
  =  tekst "[" <+> doc
 <-> verticaleLijst (map kommaErvoor docs)
 <-> tekst "]"
  where
    kommaErvoor docc = tekst "," <+> docc

verticaleLijst :: [Doc] -> Doc
verticaleLijst = foldr (<->) leeg

