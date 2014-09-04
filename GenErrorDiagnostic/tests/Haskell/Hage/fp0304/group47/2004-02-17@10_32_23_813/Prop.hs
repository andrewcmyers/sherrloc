module Prop where

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

type Bedeling = [String]

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined

evalueer                           ::Prop -> Bedeling                           -> Bool
evalueer (En props) b              = and (map (\x -> evalueer x b) props)
evalueer (Of props) b              = or (map (\x -> evalueer x b) props)
evalueer (Niet prop) b             = not (evalueer prop b)
evalueer (prop1 :-> prop2) b       = evalueer (Of [Niet prop1, prop2]) b
evalueer (Var v) b                 = or (map (eqString v) b)
evalueer (Bool bool) _             = bool



vervulbaar                         ::Prop                                       -> [Bedeling]
vervulbaar prop                    = filter (evalueer prop) (subs $ atomen prop)

  where subs []                    = [[]]
        subs (x:xs)                = map (x:) subsxs ++ subsxs
          where subsxs             = subs xs



tautologie                         ::Prop                                       -> Bool
tautologie prop                    | null vervulbaarprop               = False
                                   | otherwise                         = eqList eqString (atomen prop) (head $ vervulbaarprop)
  where vervulbaarprop             = vervulbaar prop



contradictie                       ::Prop                                       -> Bool
contradictie prop                  = null (vervulbaar prop)



toon                               ::Prop                                       -> String
toon (En [prop])                   = haakjes 3 prop
toon (En (prop:props))             = haakjes 3 prop ++ " /\\ " ++ toon props
toon (Of [prop])                   = haakjes 2 prop
toon (Of (prop:props))             = haakjes 2 prop ++ " \\/ " ++ toon props
toon (Niet prop)                   = "!" ++ haakjes 1 prop
toon (prop1 :-> prop2)             = haakjes 4 prop1 ++ " -> " ++ haakjes 4 prop2
toon (Var v)                       = v
toon (Bool b)                      | b                                 = "1"
                                   | otherwise                         = "0"
toon _                             = ""


haakjes prio prop          | prioriteit prop < prio            = "(" ++ toon prop ++ ")"
                           | otherwise                         = toon prop


  where prioriteit (_ :-> _)       = 4
        prioriteit (En _)          = 3
        prioriteit (Of _)          = 2
        prioriteit (Niet _)        = 1
        prioriteit _               = 0



equivalent                         ::Prop -> Prop                               -> Bool
equivalent prop1 prop2             = contradictie (En [prop1, Niet prop2])



deMorgan                           ::Prop                                       -> Prop
deMorgan prop                      = prop



ontleed                            ::String                                     -> Prop
ontleed str                        = Bool False



atomen                             ::Prop                                       -> [String]
atomen prop                        = verwDubbel eqString (atomenD prop)

  where atomenD (En props)         = concatMap (atomenD) props
        atomenD (Of props)         = concatMap (atomenD) props
        atomenD (Niet prop1)       = atomenD prop1
        atomenD (prop1 :-> prop2)  = atomenD prop1 ++ atomenD prop2
        atomenD (Var v)            = [v]
        atomenD (Bool _)           = []


        verwDubbel _ []           = []
        verwDubbel eq (x:xs)       | elemBy eq x xs                    = verwDubbel eq xs
                                   | otherwise                         = x : verwDubbel eq xs

-- props should be: (En props)
-- 53,72-76   55,72-76
