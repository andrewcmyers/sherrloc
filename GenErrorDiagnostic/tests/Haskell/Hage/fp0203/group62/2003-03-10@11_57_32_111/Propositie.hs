module Propositie where
data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

type Bedeling = [String]
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined

evalueer :: Prop -> Bedeling -> Bool
evalueer (Bool waarde) _ = waarde
evalueer (Niet prop) b = not(evalueer prop b)
evalueer (En []) _ = True
evalueer (En (prop:props)) b= (evalueer prop b) && (evalueer (En props) b)
evalueer (Of (prop:props)) b = (evalueer prop b) || (evalueer (Of props) b)
evalueer (Of []) _ = False
evalueer (Var string) b = elemBy eqString string b
evalueer (prop :-> propo) b = evalueer (Niet prop) b || evalueer propo b

vervulbaar:: Prop -> [Bedeling]
vervulbaar prop = filter (evalueer prop) (deelrij (verzuiver(neemBedeling prop)))

deelrij:: Bedeling -> [Bedeling]
deelrij [] = [[]]
deelrij (x:xs) = (deelrij xs) ++ (map (x:) (deelrij xs))

verzuiver:: Bedeling -> Bedeling
verzuiver [] = []
verzuiver (x:onzuiverBed) = x: verzuiver(filter (not.eqString x) onzuiverBed)

neemBedeling:: Prop -> Bedeling
neemBedeling (Bool _) = []
neemBedeling (Var string) = [string]
neemBedeling (Niet prop) = neemBedeling prop
neemBedeling (En props) = concatMap neemBedeling props
neemBedeling (Of props) = concatMap neemBedeling props
neemBedeling (prop :-> propo) = neemBedeling propo ++ neemBedeling prop



tautologie::Prop->Bool
tautologie prop | eqList (eqList eqString) (deelrij (verzuiver(neemBedeling prop))) (vervulbaar prop) = True
                | otherwise                                                                           = False




contradictie:: Prop -> Bool
contradictie prop | null (vervulbaar prop)  = True
                  | otherwise               = False



toon :: Prop -> String
toon (Bool waarde)    | waarde = "True"
                      | otherwise = "False"
toon (Var string)     = string

toon (Niet prop)      | zoekIn "\\/" prop || zoekIn "/\\" prop || zoekIn "->" prop   = "!" ++ zetHaakjes(toon prop)
                      | otherwise                                                  = "!" ++ toon prop
toon (En (props))     = kapAf(concatMap ((++ " /\\ ").toonHet ) props)
                        where toonHet p | zoekIn "\\/" p || zoekIn "->" p           = zetHaakjes(toon p)
                                        | otherwise                                = toon p
toon (Of (props))     = kapAf(concatMap ((++ " \\/ ").toonHet) props)
                        where toonHet p | zoekIn "->" p                            = zetHaakjes(toon p)
                                        | otherwise                                = toon p
toon (prop :-> propo) = toon prop ++ " -> " ++ toon propo

zoekIn:: String -> Prop -> Bool
zoekIn operator prop = (any (eqString operator) (words(toon prop)))

zetHaakjes:: String -> String
zetHaakjes string = "(" ++ string ++ ")"

kapAf::String -> String
kapAf string = reverse(drop 4 (reverse string))


equivalent:: Prop -> Prop -> Bool
equivalent prop1 prop2 = eqList (eqList eqString)
                        (filter (evalueer prop2) (deelrij(verzuiver(neemBedeling prop1))))
                        (vervulbaar prop1)
                        &&
                        eqList (eqList eqString)
                        (filter (evalueer prop1) (deelrij(verzuiver(neemBedeling prop2))))
                        (vervulbaar prop2)



deMorgan:: Prop -> Prop
deMorgan (Niet (En props))        = Of(map (deMorgan.Niet) props)
deMorgan (Niet (Of props))        = En(map (deMorgan.Niet) props)
deMorgan (prop:->propo)           = deMorgan prop :-> deMorgan propo
deMorgan (Niet(prop:->propo))     = Niet(deMorgan (prop:->propo))
deMorgan (En props)               = En(map (deMorgan) props)
deMorgan (Of props)               = Of(map (deMorgan) props)
deMorgan prop                     = prop



















doe::String -> String -> Prop
doe operator string = En[unwords((takeWhile (eqString operator) (words string))),unwords((drop 1(dropWhile(eqString operator) (words string))))]

-- should be: Var(unwords(...)), Var(unwords(...))
-- 126,26-80   126,82-143
