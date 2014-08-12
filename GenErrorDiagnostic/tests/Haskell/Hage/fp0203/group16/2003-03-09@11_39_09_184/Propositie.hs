module Propositie where
data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool
type Bedeling = [String]
eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined
weergave2 :: Prop -> String
weergave2 (Var y) = y
weerg3 :: [Prop] -> [String] -> [Prop]
weerg3 [] _ = []
weerg3 (x:xs) lijst | eqProp x
                      = (naarBool(evalueer x lijst)) : weerg3 xs lijst
                    | eqVar x
                      = (naarBool(or(test (weergave2 x) lijst))) : weerg3 xs lijst
                    | otherwise
                      = x : weerg3 xs lijst
eqVar :: Prop -> Bool
eqVar (Var _) = True
eqVar _       = False

eqProp :: Prop -> Bool
eqProp (En _)    = True
eqProp (Of _)    = True
eqProp (_ :-> _) = True
eqProp (Niet _)  = True
eqProp _         = False

weerg2 :: [Prop] -> [Bool]
weerg2 xs = foldr weergave [] xs
      where weergave (Bool y) r = y : r

naarBool :: Bool -> Prop
naarBool x = Bool x

test :: String -> [String] -> [Bool]
test _ [] = []
test x (y:ys) = (x `eqString` y) : test x ys

evalueer :: Prop -> Bedeling -> Bool
evalueer (En lijst) z = and (weerg2 (weerg3 lijst z))
evalueer (Of lijst) z = or  (weerg2 (weerg3 lijst z))
evalueer (x :-> y) z  | (or (weerg2 (weerg3 [x] z)) `eqBool` True) &&
                        (or (weerg2 (weerg3 [y] z)) `eqBool` False) = False
                      | otherwise                                   = True
evalueer (Niet x) z   = not (evalueer x z)
evalueer x z          = head (weerg2 (weerg3 [x] z))




variabelen :: Prop -> [String]
variabelen [] = []
variabelen (En(x:xs)) = weergave2 x : variabelen (En(xs))

-- [] should be (En [])
-- 59,12-13
