module Table(module List, module Table) where

import Data.List

type Table = [[String]]
eqString      :: String -> String -> Bool 
eqString = undefined

compilers :: Table
compilers =
          [["Compiler", "Universiteit/bedrijf"]
          , ["Helium","Universiteit van Utrecht"]
          , ["NHC", "University of York"]
          , ["GHC", "Microsoft Research"]
          , ["Hugs", "Galois Connections"]
          , ["Hugs.NET", "Galois Connections"]
          , ["O'Haskell", "Oregon Graduate Institute"]
          , ["O'Haskell", "Chalmers University of Technology"]
          , ["HBC", "Chalmers University of Technology"]
          ]


locaties :: Table
locaties =
  [ ["Universiteit/bedrijf", "Land", "Stad"]
  , ["Universiteit van Utrecht", "Nederland", "Utrecht"]
  , ["University of York", "Engeland", "York"]
  , ["Microsoft Research", "Engeland", "Cambridge"]
  , ["Galois Connections", "Verenigde Staten", "Beaverton"]
  , ["Oregon Graduate Institute", "Verenigde Staten", "Beaverton"]
  , ["Chalmers University of Technology", "Zweden", "Goteborg"]
  ]

ontlijst :: [String] -> String -> String
ontlijst([x]) s = x ++ s

ontlijst (x:xs) s = x ++ s ++ ontlijst(xs) s


ontlijstsec :: [String] -> String
ontlijstsec([x]) = x
ontlijstsec (x:xs) = x ++ ontlijstsec(xs)


writeTable :: Table -> String

writeTable ([xs]) = ontlijst(xs) "|"


writeTable (xs:xss) = kader(compilers) ++ writeRecord [xs] ++ kader(compilers) ++ writeRecord xss ++ kader(compilers)


writeRecord :: Table -> String
writeRecord ([xs]) = ontlijst(xs) "|" ++ "\n"
writeRecord (xs:xss) = ontlijst(xs) "|" ++ "\n|" ++ writeRecord xss


kolombreedte :: [String] -> Int
kolombreedte []=0
kolombreedte lijst = maximum(map length lijst)


kader :: Table  -> String

kader [] = "&&\n"
kader t = "+" ++ ontlijstsec(replicate (kolombreedte(head(transpose t))) "-") ++ "+" ++ kader2(tail(transpose(t)))

kader2 :: Table -> String
kader2 [] = "\n|"
kader2 t = ontlijstsec(replicate(kolombreedte(head(transpose t))) "-") ++ "+" ++ kader2(tail(transpose t))







































project :: [String] -> Table-> Table







project _ [] = []





project s (t:ts) | eqString f h  = t:p
                 | not(eqString f h)  = p
                 where h = head(transpose t)
                       f = ontlijstsec s
                       p = project s ts








select :: String -> (String -> Bool) -> Table -> Table




select _ _ [] = []

select veld conditie tabel |eqString veld (hh) = zoek conditie (th)
                           |not(eqString veld (hh)) = select veld conditie (tail(tabel))
                           where th = tail(head(transpose tabel))
                                 hh = head(head(transpose tabel))


zoek :: (String -> Bool) -> [String] -> [String]
zoek _ [] = []
zoek cond lijst = filter (cond) lijst

-- missing []
-- 144,50-67
