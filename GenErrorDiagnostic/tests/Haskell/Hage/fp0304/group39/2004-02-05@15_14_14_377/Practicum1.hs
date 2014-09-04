module Practicum1 where

import Data.List

type Table = [[String]]
type TableLine = [String]

eqString      :: String -> String -> Bool 
eqString = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined

compilers :: Table
compilers =
  [ ["Compiler", "Universiteit/bedrijf"]
  , ["Helium", "Universiteit van Utrecht"]
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

soort :: Table
soort =
  [ ["Universiteit/bedrijf", "Soort"]
  , ["Universiteit van Utrecht", "Universiteit"]
  , ["University of York", "Universiteit"]
  , ["Microsoft Research", "Bedrijf"]
  , ["Galois Connections", "Bedrijf"]
  , ["Oregon Graduate Institute", "Universiteit"]
  , ["Chalmers University of Technology", "Universiteit"]
  ]






columnMaxLength :: Table -> Int -> Int
columnMaxLength [] _ = error "practicum1.columnMaxLength: type error"
columnMaxLength [x] n = length (x !! n)
columnMaxLength (x:xs) n = max (length (x !! n)) (columnMaxLength xs n)



setColumnLengths :: Table -> [Int]
setColumnLengths [] = error "practicum1.setColumnLength: type error"
setColumnLengths table@(x:_) = sclLoop table 0 (length x -1)






sclLoop :: Table -> Int -> Int -> [Int]
sclLoop table n l | n > l = error "practicum1.sclLoop: n > length(table)"
                  | n == l = [cml]
                  | otherwise = cml : sclLoop table (n+1) l
                              where cml = columnMaxLength table n




brakeLine :: [Int] -> String
brakeLine [] = "+"
brakeLine (x:xs) = ('+':replicate x '-') ++ brakeLine xs



tableCell :: Int -> String -> String
tableCell lnth str | lnth < length str = error "Practicum1.tableCell: field length < string"
                   | otherwise  = str ++ replicate (lnth - length str) ' '




tableLine :: [Int] -> TableLine -> String
tableLine [] [] = "|"
tableLine (x:xs) (y:ys) = ('|':tableCell x y) ++ tableLine xs ys
tableLine _  _ = error "Practicum1.tableLine: list lengths do not match"



tableBody :: [Int] -> Table -> String
tableBody _ [] = []
tableBody lnth (t:able) = tableLine lnth t ++ "\n" ++ tableBody lnth able



writeTableString :: [Int] -> Table -> String
writeTableString _ [] = error "Practicum1.writeTableString: table is empty"
writeTableString lnth (t:able) | not(eqString "" tblBody) = tblHead ++ tblBody ++ brkln ++ "\n"
                               | otherwise                = tblHead
                                         where brkln   = brakeLine lnth
                                               tblBody = tableBody lnth able
                                               tblHead = brkln ++ "\n" ++ tableLine lnth t ++ "\n" ++ brkln ++ "\n"




writeTable :: Table -> String
writeTable table = writeTableString (setColumnLengths table) table


putTable :: Table -> IO ()
putTable table = putStr (writeTable table)





testColumn :: String -> TableLine -> Int
testColumn string [] = 1 - length string
testColumn string (t:ableln) | eqString string t = 1
                             | otherwise         = 1 + testColumn string ableln




projectColumnNumbers :: [String] -> TableLine   -> [Int]
projectColumnNumbers []        _      = []
projectColumnNumbers (v:elden) header = testColumn v header : projectColumnNumbers elden header



projectLine :: [Int] -> TableLine -> TableLine
projectLine []        _     = []
projectLine (0:alues) tableln = projectLine alues tableln
projectLine (v:alues) tableln = (tableln !! (v-1)) : projectLine alues tableln



projectNumbers :: [Int] -> Table -> Table
projectNumbers _      []       = []
projectNumbers values (t:able) = projectLine values t : projectNumbers values able



project :: [String] -> Table -> Table
project _      []          = error "Practicum1.project: table is empty"
project string table@(t:_) = projectNumbers (projectColumnNumbers string t) table









selectRecordNumbers :: Int -> (String -> Bool) -> Table -> [Int]
selectRecordNumbers _        _     []       = []
selectRecordNumbers columnnr testf (t:able) | testf (t!!(columnnr)) = beginnr: restlist
                                            | otherwise             = restlist
                                                        where  beginnr  = 0
                                                               restlist = map (+1) (selectRecordNumbers columnnr testf able)






selectColumnNumber :: String -> TableLine -> Int
selectColumnNumber _         []         = error "Practicum1.selectColumnNumber: no matches found"
selectColumnNumber fieldname (t:ableln) | eqString fieldname t = beginnr
                                        | otherwise         = 1 + selectColumnNumber fieldname ableln
                                              where beginnr = 0



selectRecords :: [Int] -> Table -> Table
selectRecords []         _     = []
selectRecords (r:ecords) table = (table!!r):selectRecords ecords table




select :: String -> (String -> Bool) -> Table -> Table
select _         _     []       = error "Practicum1.select: table is empty"
select fieldname testf (t:able) = t: selectRecords (selectRecordNumbers (selectColumnNumber fieldname t) testf able) able






join :: Table -> Table -> Table
join [] [] = []

join t1 t2 = [x++y|x<-t1,y<-t2, tf x y]
             where
             tf l1 l2 = eqString (select index1 l1) (select index2 l2)
             select i lijst = head(drop i lijst)
             index1 = vindIndex (head t1) 0
             index2 = vindIndex (head t2) 0
             vindIndex [] t = 0
             vindIndex (x:xs) t | eqString x gemVeld = t
                                | otherwise = vindIndex (xs) (t+1)
             gemVeld = headExtract (head t1) (head t2)
             headExtract l1 l2 = head [x y |x<-l1, y<-l2, eqString x y]











query :: Table
query = project ["Compilers"]
         (select "Soort" (eqString "Universiteit")
          (join compilers (join locaties soort)))





joinRecords :: Table -> Table -> Table
joinRecords (t:able1) (ta:ble2) | eqTableLine t ta = t: (able1 ++ ble2)
                                | otherwise        = error "Practicum1.joinRecords: Table headers do not match"
joinRecords _         _        =  error "Practicum1.joinRecords: tables are empty"






matchHeight :: Table -> Table -> Table
matchHeight []           _      =  error "Practicum1.matchHeight: table is empty"
matchHeight table1@(t:_) table2 | lt1 < lt2 = table1 ++ generateTable (lt2 - lt1) (length t)
                                | otherwise = table1
                                        where lt1 = length table1
                                              lt2 = length table2


matchWidth :: Table -> Table -> Table
matchWidth []        []        = []
matchWidth (t:able1) (ta:ble2) | lt < lta  = ( t ++ generateTableLine (lta - lt) ) : matchWidth able1 ble2
                               | otherwise = t : matchWidth able1 ble2
                                    where lt  = length t
                                          lta = length ta
matchWidth _        _         = error "Practicum1.matchWidth: table height does not match"



eqTable :: Table -> Table -> Bool
eqTable table1 table2 = eqList (eqList eqString) table1 table2

eqTableLine :: TableLine -> TableLine -> Bool
eqTableLine tableln1 tableln2 = eqList eqString tableln1 tableln2






eqTableSize :: Table -> Table -> Bool
eqTableSize []        []        = True
eqTableSize (t:able1) (ta:ble2) | length t == length ta = eqTableSize able1 ble2
                                | otherwise             = False
eqTableSize _         _         = False




generateTableLine :: Int -> TableLine
generateTableLine 0 = []
generateTableLine n = [] : generateTableLine (n-1)

generateTable :: Int -> Int -> Table
generateTable 0      _     = []
generateTable height width = generateTableLine width : generateTable (height-1) width

-- should delete: y
-- 214,40-42   214,42-42
