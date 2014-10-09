module PpHTML (module PpHTML, module Pretty, module Maybe) where

import Pretty
import Data.Maybe




type Attributen = [(String, String)]
type Tagnaam = String
data HTML
     = Tekst
     | EenTag Tagnaam Attributen

     | TweeTags Tagnaam Attributen [HTML]





simpel :: HTML -> Bool
simpel Tekst = True
simpel (EenTag _ _)= True
simpel (TweeTags _ attributen html) | (length attributen) + (length html) == 1 && (simpel head(html)) = True
                                    | otherwise = False

-- (simpel head(html should be simpel (head(html
-- 24,84-100    24,84-101
