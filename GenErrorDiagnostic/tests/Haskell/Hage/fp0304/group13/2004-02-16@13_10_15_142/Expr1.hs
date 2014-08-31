module Expr1 where

infixl 7 :*:
infixl 7 :/:
infixl 6 :+ ,:-:
infixl 9 :/\:,:\/:,:->:
infixl 9 :<->:

eqString      :: String -> String -> Bool 
eqString = undefined
eqChar      :: Char -> Char -> Bool
eqChar = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

data Expr = Con Float

          |Var String
          |Expr :+ Expr
          |Expr :-: Expr
          |Expr :*: Expr
          |Expr :/: Expr


diff             :: Expr->String->Expr
diff (Con _) _  = Con 0.0
diff (Var x) dx  |eqString x dx = Con 1.0
                 |otherwise = Con 0.0
diff (f :+ g) dx = diff f dx :+ diff g dx
diff (f :-: g) dx = diff f dx :-: diff g dx
diff (f :*: g) dx = f :*: diff g dx :+ g :*: diff f dx
diff (f :/: g) dx = (g :*: diff f dx :-: f :*: diff g dx) :/: (g :*: g)



simple                         :: Expr -> Expr


simple ((Con 1.0) :*: g )        =  simple g
simple (f       :*: (Con 1.0))   =  simple f
simple (f :*: g)               = (simple f :*: simple g)
simple   f                     = f


data Prop = Cons Bool
          | Vari Char
          | Not  Prop
          | Prop :/\:  Prop
          | Prop :\/:  Prop
          | Prop :->:  Prop
          | Prop :<->: Prop


logica                           :: Prop  -> Bool
logica (Vari  b1 )               |eqChar b1 'T'     = True
                                 |otherwise          = False
logica (Cons bool1)              |eqBool bool1 True  = True
                                 |otherwise          = False
logica (Not p)                   = not (logica p)
logica (p1 :/\:  p2)             = logica p1 && logica p2
logica (p1 :\/:  p2)             = logica p1 || logica p2
logica (p1 :->:  p2)             = logica (Not p1) || logica p2
logica (p1 :<->: p2)             = logica (p1 :->: p2)
                                   && logica (p2 :->:p1)



data Boom3 a b = Tak3 a (Boom3 a b) (Boom3 a b)
               | Blad3 b









tb :: Boom3 String (String, Int)
tb =  Tak3 "m" (Tak3 "n" (Blad3 ("o",12345))
                         (Blad3 ("p",12345))
                )
               (Tak3 "l" (Blad3 ("a",25415))
                         (Blad3 ("c",25415))
                )


filter1             :: (a->Bool)-> [a]->[a]
filter1 p           = concat map box
                      where box y |p y = [y]
                                  |otherwise = []

-- concat map should be: concat.map
-- 89,23-28   89,23-36
