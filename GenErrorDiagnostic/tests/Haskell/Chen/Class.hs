-- Improving type error diagnosis

-- Problem: len or 10 needs an annotation
class Mul a b c | a b -> c where
    mul :: a -> b -> c
    
instance Mul Int Int Int where
    mul = (*)

f xs = let len = length xs
       in len `mul` 10
zero = f []

-- Problem: Functional dependencies causes error, 'a' should have type Bool
class G a b | b -> a where
    g :: a -> b
    
instance G Bool Bool

f2 = not (g (g 'a'))

-- Problem: 'a' and True should have the same type
class Collects ce e | ce -> e where
    empty :: ce
    insert :: e -> ce -> ce
    
f c = insert 'a' (insert True c)

-- Problem: the use of > causes Eq a to be insufficient
notNull :: Eq a => [a] -> Bool
notNull xs = xs > []

-------------------------
-- Type inference and type error diagnosis

-- Problem: empty's type is always ambiguous
class Collects c e where
    insert :: e -> c -> c
    lookup :: c -> Int -> Maybe e
    empty :: c
    
init e = insert e empty

-- Problem: the arguments to c must have the same type
class C a b | a -> b where
    c :: a -> b -> a
    
instance C a a

e = c (\x -> x) 'a'

-- inferred type doesn't subsume declared type
notEq :: Eq a => a -> b -> Bool
notEq x y = if x == y then False else True


----------------------------------
-- Top quality type error messages

-- Problem: only Int is defined as the Similar instance.
-- For more information, refer to page 201
class Similar a where
    (<=>) :: a -> a -> Bool
    
instance Similar Int where
    (<=>) = (==)

f x xs = [x] == xs
    