module Cluedo ( module Cluedo, module Similarity) where
import Similarity

readInt   :: String -> Int
readInt = undefined
eqTuple2 :: (a -> a -> Bool) -> (b -> b -> Bool) -> ((a,b) -> (a,b) -> Bool)
eqTuple2 = undefined

main :: IO ()
main = do putStr "Hoe heet je?"
          naam <- getLine
          putStrLn ("Hoi, " ++ naam)

          putStr "Welke speler ben je? Mustard, Plum, Green, Peacock, Scarlett of White? "
          antwoord <- getLine
          verwerkAntwoord antwoord

          putStr "Welke kaarten heb je?"
          eigenkaarten <- getLine
          putStrLn  eigenkaarten






          putStr "Wat zijn de startposities van de spelers?, Voer coordinaten speler in:  Wat is het verticale component?  "
          coordinaten <- getLine
          putStrLn coordinaten
          putStr "Wat is het horizontale component? "







verwerkAntwoord :: String -> IO ()
verwerkAntwoord  "Scarlett" = putStrLn ("Ok Mej. Scarlett, U bent speler 1")
verwerkAntwoord  "Mustard"  = putStrLn ("Ok Kolonel Mustard, U bent speler 2")
verwerkAntwoord  "Green"    = putStrLn ("Ok Dominee Green, U bent speler 3")
verwerkAntwoord  "Plum"     = putStrLn ("Ok Prof. Plum, U bent speler 4")
verwerkAntwoord  "Peacock"  = putStrLn ("Ok Mevr. Peacock, U bent speler 5")
verwerkAntwoord  "White"    = putStrLn ("Ok Mevr. White, U bent speler 6")


controle :: String -> [String]
controle y = controleerKaarten (words y)


controleerKaarten ::  [String] -> [String]
controleerKaarten [] = []
controleerKaarten (k:ks) = (corrigeerInvoer k lijstmetallekaarten) : controleerKaarten ks

lijstmetallekaarten :: [String]
lijstmetallekaarten = ["Dolk", "Kandelaar", "Revolver", "Touw" , "Loden Pijp", "Steeksleutel",
                        "Hal","Eetkamer", "Studeerkamer", "Serre", "Zitkamer", "Keuken", "Bibliotheek", "Danszaal", "Biljartzaal",
                        "Mustard", "Plum", "Green", "Peacock", "Scarlett", "White"]


corrigeerInvoer:: String -> [String] -> String
corrigeerInvoer k p = concat(filter (not.eqString " ")(map zoekgelijke (zip (map (similar k) p) p)))

zoekgelijke :: (Bool, String) -> String
zoekgelijke y | eqBool (fst y)  True = snd y
              | otherwise = " "



omzetCoord :: String -> String -> (Int,Int)
omzetCoord x y = ((readInt x),(readInt y))


roomslist = [("Studeerkamer",[(7,4)]),("Serre",[(10,5),(12,7),(13,7)]),("Zitkamer",[(18,6)]),("Eetkamer",[(18,10),(17,13)]),
             ("Biljartzaal",[(20,19)]),("Danszaal",[(10,18),(15,18),(9,20),(16,20)]),("Hal",[(5,20)]),
             ("Keuken",[(2,13),(6,16)]),("Bibliotheek",[(7,9),(4,11)])]





checkKamer x = concat (filter (not.eqString " ")(map (check x) roomslist))







geefKamer :: (Int, Int) -> IO ()
geefKamer y | eqString  " " checkKamer y = putStrLn (" Op deze positie bevindt je je niet in een kamer" )
            | otherwise = putStrLn ("Deze speler bevindt zich in de kamer " ++ (checkKamer y))




check :: (Int,Int) -> (String, [(Int,Int)]) -> String
check x y | elemBy (eqTuple2  (==) (==)) x (snd y) = fst y
          | otherwise = " "

-- missing () round checkKamer y
-- 91,29-38
