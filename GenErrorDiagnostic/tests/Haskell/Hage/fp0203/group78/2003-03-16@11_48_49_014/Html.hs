module Html where

data HTML = Tag       String [HTML]
          | SingleTag String
          | Tekst     String

vb :: HTML
vb
  = [ SingleTag "Hallo" ]

-- should delete []
-- 9,5-25
