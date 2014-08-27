module Pretty where







infixl 4 <->
infixl 5 <|>






type Doc = [String]







leeg     :: Doc






tekst    :: String -> Doc






















(<|>)    :: Doc -> Doc -> Doc





















(<->)    :: Doc -> Doc -> Doc





springIn :: Int -> Doc -> Doc










layout   :: Doc -> IO ()




leeg = []


tekst string
  = [ string ]


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


boven <-> onder
  = boven ++ onder


springIn n regels
  = map spatiesErvoor regels
  where
    spatiesErvoor regel = spaties n ++ regel


layout doc
  = putStr (unlines doc)



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
