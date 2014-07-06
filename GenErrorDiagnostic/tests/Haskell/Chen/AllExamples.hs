

-- Problem: missing parentheses around "a" ++ "b" 
v1 = print "a" ++ "b"

-- 6,12-21


-- Problem: should use square brackets rather than parentheses
v2 = sum (5,6,7)

-- 6,10-16


-- Problem: should use \ x y -> ...
v3 = zipWith (\(x,y) -> x + y) [1,2] [3,4,5]          

-- 6,16-20


-- Problem: True should be something of Int, "hi" should be something of Int
v4 = let x = 3 + True in 4 + "hi"                    

-- 6,18-21 6,30-33 


-- Problem: "a" and ["b"] should be swapped
addList ls s = if s `elem` ls then ls else s : ls
v5 = addList "a" ["b"]                                 

-- 7,14-22 


-- Problem: (nth search) misses an argument of type Int
nth = (!!)
final moves idx search = 
    if (idx == moves -1) 
    then []
    else (nth search) : final moves (idx+1) search ++ search


---------------
-- repairing type errors in functional programs    

--10,10-21 


-- Problem: [1,2,3] and show should be swapped
v7 = map [1,2,3] show             
 
-- 6,10-16


-- Problem: "1" should be 1
one = "1"
two = one + one
three = two + one
four = three + one
five = four + one

-- 6,7-9


-- Problem: zero and addReciprocals should be swapped
foldleft = foldl                       
intList = [12, 3]
zero = 0.0
addReciprocals total i = total + (1.0 / i)
totalOfReciprocals = foldleft zero addReciprocals intList

---------------------------------------------
-- Compositional Explanation of Types
--   and Algorithmic Debugging of Type Errors        

-- 10,31-49


-- Problem: x in the 2nd line should be [x]
reverse1 [] = []                                       
reverse1 (x:xs) = reverse1 xs ++ x
last1 xs = head (reverse1 xs)
init1 = reverse1 . tail . reverse1
rotateR xs = last1 xs : init1 xs

-- 7,34-34


-- Problem: xs should be pushed into (++) as its one argument
f xs ys = ((map toUpper) . (++)) xs ys          

-- 6,28-31


-- Problem: there are several ways to fix the program. No oracle given.
f1 xs = let g y = y : xs                         
        in g 1 ++ g True


----------------------------------------------
-- Explaining Type Errors by Finding the Source of Type Conflict

-- 7,14-14


-- Problem: there are several ways to fix the program. No oracle given.
f2 x = map x [x+2]                             

-- 6,15-15


-- Problem: there are several ways to fix the program. No oracle given.
v10 = (\x -> x + 1) ((\y -> if y then True else False) False)
    
------------------------------------------
-- Helium, for Learning Haskell                    

-- 6,14-14  6,16-16


-- Problem: should be sin 0.2
test = sin .2                                                              
            
-- 6,12-13 


-- Problem: there are several ways to fix the program. No oracle given.            
makeEven x = if even x then True else x+1       

-- 6,17-20  6,29-32  6,39-41


-- Problem: there are several ways to fix the program. No oracle given.
test = \f i -> (f i, f 2, [f,i])                

-- 6,28-30


-- Problem: [1..10] and even should be swapped
test = map [1..10] even                         

-- 6,12-23


-- Problem: there are several ways to fix the program. No oracle given.
test = xs : "def"
       where xs = "abc"

-- 6,13-17


-- Problem: max should be maximum
maxLength xs = max (map length xs) + 0           

--------------------------------------
-- Improving Error Messages in Type System(2010)   

-- 6,16-18


-- Problem: [1] and True should be Int
v12 = [1,[1],True]                          

-- 6,8-17


-- Problem: True should be Int
v13 = (\x -> x + 4) (if True then True else 1)        

-- 6,35-38


-- Problem: there are several ways to fix the program. No oracle given.            
v14 = (\x -> x + (x 4)) (if True then True else 1)    

-- 6,14-14   6,16-16   6,39-42  6,49-49


-- Problem: x should be of even x
v15 = (\x -> [x,True,False,True]) (if True then 1 else 2)

-- 6,15-15


-- Problem: 1 should be of type Bool
v16 = (\x -> [1,x,True])                              

-- 6,15-15


-- Problem: x in condition should be even x or odd x.            
v17 = (\x -> x + (if x then 1 else 2))                

-- 6,22-22


-- Problem: there are several ways to fix the program. No oracle given.            
v18 = if True then \f -> f 1 2                        
              else \g -> g True False

-- 6,28-28


-- Problem: w should be even w, True should be of type Int        
v19 = \x -> let w = x +1                              
            in if w then [1,True] else [4]

----------------------------------------
-- Improving Type Error Diagnosis
-- (Not including examples regarding type annotations and type classes)

-- 7,19-19


-- Problem: x should be [x]
insert x [] = x                                 
insert x (y:ys) | x > y = y : insert x ys
                | otherwise = x : y : ys
       
-- 6,15-15


-- Problem: y e should be e y       
f c = if c then \ g x -> g x                    
           else \ e y -> y e

-- 7,26-28 


-- Problem: "True" should be True
string2bool :: String -> Bool
string2bool = undefined
not x = if x then string2bool "False"           
             else fst ("True", 1)
             
-- 9,24-29 


-- Problem: there are several ways to fix the program. No oracle given.            
f 'a' b z = error "’a’"                         
f c True z = error "True"
f x y z = if z then x else y
f x y z = error "last"

-- 8,28-28


-- Problem: + should be ++
sumLists = sum2 . map sum2                       
sum2 [] = []
sum2 (x:xs) = x + sum2 xs

-----------------------------------------
-- Interactive type debugging in Haskell          

-- 8,17-17


-- Problem: the parameter True should be of type Char
f True = True                                   
p = f 'a'

-- 6,3-6


-- Problem: x < y should be x < y z
q x y z = if x < y then z else y z              

-- Type safe in Haskell


-- Problem: [z] should be z
fold f z [] = [z]                             
fold f z (x:xs) = fold f (f z x) xs
flip1 f x y = f y x
reverse2 = fold (flip1 (:)) []
palin xs = reverse2 xs == xs

----------------------------------
-- Program-Update Inference Through Type-Change Constraints

-- 6,15-17


-- Problem: succ should be (+)
v21 = \n -> foldl succ 0 n                             

-- 6,19-22 


-- Problem: there are several ways to fix the program. No oracle given.            
v22 = \a -> \b -> (if True then a else b,
                   \c -> b c,
                   b (\x -> \y -> y ) 1 True,
                   (a 1)+1)

-- 9,23-23  9,25-25


-- Problem: there are several ways to fix the program. No oracle given.            
v23 = \f g a -> (f a, f 1, g a, g True)                

-- 6,25-25  6,35-38


-- Problem: there are several ways to fix the program. No oracle given.            
v24 = \x -> let g = \y -> (y : x) in g 1 ++ g True         

-- 6,40-40  6,47-50


-- Problem: there are several ways to fix the program. No oracle given.
v25 x = if x then succ x else x                          

-- Type safe in Haskell


-- Problem: there are several ways to fix the program. No oracle given.            
v26 = (\x -> succ x) ((\y -> if y then True else True) True) 

-------------------------------
-- Top Quality Type Error Messages

-- Type safe in Haskell


-- Problem: foldr (+) xs should be foldr (+) 0 xs
f = \xs -> (length xs, foldr (+) xs + 0)      

-- 6,34-35


-- Problem: False should be "False"
showInt x =                                         
    case x of
        0 -> False
        1 -> "one"
        2 -> "two"
        _ -> "many"

-- 8,14-18


-- Problem: "b" should be b
test b = if "b" then "yes!" else "no!"            

-- 6,13-15


-- Problem: 0 and (+) should be swapped
sumInt is = foldr 0 (+) is                        

-- Type safe in Haskell


-- Problem: [1,n] and (^2) should be swapped
squareList n = map [1, n] (^2)             

-- 6,20-30


-- Problem: not i == 0  should be not ( i == 0 )
isZero :: Int -> Bool                             
isZero i = not i == 0

-- 7,16-21


-- Problem: xs and 0 should be swapped
sumFloat :: [Float ] -> Float                     
sumFloat xs = foldr (+) xs 0 + 0.0

-- 7,25-28


-- Problem: map (+1) needs an argument of type [Int]
incrementList :: [Int] -> [Int]                   
incrementList xs = map (+1) ++ [1]

-- 7,20-27


-- Problem: (-1) should be ((-) 1)
decrementList :: [Int ] -> [Int ]                 
decrementList xs = map (-1) xs

-- 7,24-27


-- Problem: there are several ways to fix the program. No oracle given.            
test c = if c then [1 .. 10] else "abc"          

-- 6,20-28  6,35-39


-- Problem: there are several ways to fix the program. No oracle given.            
f x y = [x , y, id , "\n"]                  

-- 6,17-18   6,22-25


-- Problem: (x,xs) should be (x:xs)                   
maxOfList [ ] = error "empty list"
maxOfList [x] = x
maxOfList (x , xs) = x `max` maxOfList xs


-- 8,11-18


-- Problem: right of <$> is not a Parser type
type Parser s a = [s ] -> [(a, [s ])]             
(<$>) :: (a -> b) -> Parser s a -> Parser s b
(<$>) = undefined

(<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
(<*>) = undefined

symbol :: Char -> Parser Char Char
symbol = undefined

token :: String -> Parser Char String
token = undefined

option :: Parser s a -> a -> Parser s a
option = undefined


test11 :: Parser Char String                        
test11 = map toUpper <$> "hello, world!"

-- 24,26-40


-- Problem: type of (++) doesn't match String -> Char -> String
type Parser s a = [s ] -> [(a, [s ])]             
(<$>) :: (a -> b) -> Parser s a -> Parser s b
(<$>) = undefined

(<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
(<*>) = undefined

symbol :: Char -> Parser Char Char
symbol = undefined

token :: String -> Parser Char String
token = undefined

option :: Parser s a -> a -> Parser s a
option = undefined


test12 :: Parser Char String                        
test12 = (++) <$> token "hello world"
              <*> symbol '!'

-- 24,10-13


-- Problem: "" and token "hello" should be swapped
type Parser s a = [s ] -> [(a, [s ])]             
(<$>) :: (a -> b) -> Parser s a -> Parser s b
(<$>) = undefined

(<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
(<*>) = undefined

symbol :: Char -> Parser Char Char
symbol = undefined

token :: String -> Parser Char String
token = undefined

option :: Parser s a -> a -> Parser s a
option = undefined


test13 :: Parser Char String                        
test13 = option "" (token "hello!")

-- 24,17-35


-- Problem: [1 .. 5] and ((>10) . (^2)) should be swapped
v30 = map [1 .. 5] ((>10) . (^2))

-- 6,11-33


-- Problem: 0 and xs should be swapped
f xs = 0 !! xs                                    

-- Type safe in Haskell


-- Problem: there are several ways to fix the program. No oracle given.            
f x = if x then x + 1 else x                     


-------------------------------------
-- Type inference and type error diagnosis                             

-- 6,10-10


-- Problem: x should be [x]
split xs = case xs of                                               
            [] -> ([],[])
            [x] -> ([], x)
            (x:y:zs) -> let (xs, ys) = split zs
                        in (x:xs, y:ys)

-- 8,25-25


-- Problem: [] should be 0
sumLengths [] = []                                                  
sumLengths (xs:xss) = length xs + sumLengths xss

-- 6,17-18


-- Problem: the condition x should be something like isLower x 
f x = (if x then (toUpper x) else (toLower x))                      

-- 6,11-11


-- Problem: y e should be e y 
f c = if c then \ g x -> g x                                        
           else \ e y -> y e

-- 7,26-28


-- Problem: there are several ways to fix the program. No oracle given.            
idEq = \ x -> x == [x]

-- 6,15-15  6,20-22 


-- Problem: True should be something of type Char -> a
map1 f [] = []                                                       
map1 f (x:xs) = f x : map1 f xs
test = map1 True ['a','b','c']

-- 8,13-16 


-- Problem: ord should be something has argument type Bool
f x ys = ord (if x then x else null ys == x)                        

-- 6,10-12


-- Problem: doRow r ys should be doRow ys r
plot f dx dy oy =                                                   
    let fxs = getYs f dx
        ys = map (\y-> fromIntegral (y-oy)*dy) [maxY,maxY-1..minY]
        rows = map (doRow fxs) ys
    in unlines rows
        where
        doRow [] r = ""
        doRow (y:ys) r = (if y < r && y > (r-dy) then '*'
                            else ' ') : doRow r ys
        getYs f dx = [ f ((centre x * dx)) | x <- [minX..maxX] ]
            where centre = (+) .5
minX = 0
maxX = 79
minY = 0
maxY = 19

-- 14,41-50


-- Problem: there are several ways to fix the program. No oracle given.            
f g x y = (g (if x then x else y), g "abc")

-- 6,25-25  6,38-42


-- Problem: pop requires a pair while push returns a list
idStack stk = pop (push undefined stk)                              
push top stk = (top:stk)
pop (top,stk) = stk
empty = []

-- 6,15-38  7,16-24


-- Problem: biggest should of type Int, rather than [Int] -> Int
normalise xs = scale biggest xs                                   
scale x ns = map (/x) ns
biggest (x:xs) = max x xs
            where max x [] = x
                  max x (y:ys) | y > x = max y ys

-- Type safe in Haskell


-- Problem: [xs] should be xs
merge [] ys = ys                                                    
merge xs [] = [xs]
merge (x:xs) (y:ys) | x < y = [x] ++ merge xs (y:ys)
                    | otherwise = [y] ++ merge (x:xs) ys

-- 7,15-18


-- Problem: there are several ways to fix the program. No oracle given.            
v40 = \f x y z -> (f y, y == ('a',x), f (z,True))

-- Type safe in Haskell


-- Problem: there are several ways to fix the program. No oracle given.                
-- DZ: added the defnition of "plus"
plus :: Int -> Int -> Int
plus = (+)

v41 = \a -> plus ((\b -> if b then b else a) True, 3)

-- 10,18-53


-- Problem: ++ should be :
rever = rev []                                                      
    where rev rs [] = rs
          rev rs (x:xs) = rev (x++rs) xs
v42 = rever [1,2,3]

---------------------
-- Better Type Error Messages Through Lazy Typings

-- 8,33-34


-- Problem: [0] should be 0
f x = case x of                                                     
    0 -> [0]
    1 -> 1
plus :: Int -> Int -> Int
plus = (+)
fib x = case x of
    0 -> f x
    1 -> f x
    n -> fib (n-1) `plus` fib (n-2)

-- 7,10-12


-- Problem: x should be [x]
split xs = case xs of                                               
    [] -> ([],[])
    [x] -> (x,[])
    (x:y:zs) -> let (xs, ys) = split zs
                in (x:xs, y:ys)

-- 8,13-13


-- Problem: "abc" and toLower should be swapped
map2 f [] = []                                                      
map2 f (x:xs) = f x : map2 xs
test = map2 "abc" toLower

-- 8,13-25


-- Problem: False should be something of Int
v50 = (\x->x+3) (if True then False else 1)                               

-- 6,31-35


-- Problem: x in the first line should be [x]
insert x [] = x                                                     
insert x (y:ys) 
    | x > y = y : insert x ys
    | otherwise = x : y : ys

-- 6,15-15


-- Problem: ["error"] and "location" should be swapped
add1 str lst                                                         
    | str `elem` lst = lst
    | True = str:lst
v51 = add1 ["error"] "location"

-- 9,12-31


-- Problem: [lst] should be lst
add2 str lst                                                         
    | str `elem` lst = [lst]
    | True = str:lst
v = add2 "error" ["location"]

-- 7,24-28


-- Problem: 2 should be Bool or True should be Int
h = if True then (\f-> f (f 2))                                     
            else (\g-> g (g True))

-- 6,29-29  7,29-32


-- Problem: 2 should of type Bool
v = let h = if True then (\f-> f (f 2))                             
                    else (\g-> g (g True))
    in h not

-- 6,37-37


-- Problem: [0] should be 0 and True should of type Int
f x = case x of                                                           
    0 -> [0]
    1 -> 1
plus :: Int -> Int -> Int
plus = (+)
fib x = case x of
    0 -> f x
    1 -> True
    n -> fib (n-1) `plus` fib (n-2)

-- 7,10-12


-- Problem: first not should be of type Int -> Int, second not should
-- be of type Bool -> Int
ff x = if True then not x else x + 1                                 
gg x = if True then not x else 2
v55 = ff 3 + gg True

-- 7,21-25  8,21-25


-- Problem: 1 should be of type [Int]
f x = case x of                                                     
    0 -> [0]
    1 -> 1
fib x = case x of
    0 -> f x
    1 -> f x
    n -> head (f x)

------------
-- Discriminative Sum Types Locate the Source of Type Errors

-- Type safe in Haskell


-- Problem: there are several ways to fix the program. No oracle given.                
v56 = (\f -> f 1) (\y -> if y then 1 else 0)                           

----------
-- Proofs about a Folklore Let-Polymorphic Type Inference Algorithm

-- 6,16-16


-- Problem: second == should have type Int -> Int -> Int
fac n = if n ==0 then 1 else n * fac (n==1)             

-----------
-- type error reporting in the hindley milner system

-- 6,39-42


-- Problem: + should be ++
sumLengths [] = []
sumLengths (xs:xss) = xs + sumLengths xss        

-- Type safe in Haskell


-- Problem: [] should be 0
sum' [] = []                                            
sum' (x:xs) = x `plus` sum' xs
plus = (+) :: Int -> Int -> Int

-- 6,11-12


-- Problem: True should be of type Char -> a
map' f [] = []                                          
map' f (x:xs) = f x ++ map' f xs
test = map True ['a','b','c']

-----------
-- yj.phd.pdf

-- 8,12-15


-- Problem: 3 should be of type Bool
v60 = \f y x -> f (y x) (y 3) (not x)                         

-- 6,28-28


-- Problem: there are several ways to fix the program. No oracle given.                
v61 = \x -> if x then succ x else 2                           

-- 6,35-35  6,23-28


-- Problem: there are several ways to fix the program. No oracle given.                
v62 = \x -> if x > 0 then x else 0.0 -x                       

-- Type safe in Haskell


-- Problem: x in the list should be of type Int
v63 = \x a -> (x a, x 2, [x, 2])                              
             
-- 6,27-27


-- Problem: the variable t has type [a] should be of type a
m f (h:t) = f h : [f t]                                   

-- 6,22-22


-- Problem: 3 should be of type Int -> a
v64 = let f = \x -> let y = x                                 
              in y 5
      in f 3

-- 8,12-12


-- Problem: there are several ways to fix the program. No oracle given.                
v65 = \x -> let y = (\z -> let f = x z                       
                           in \w -> w)
            in (y 5, y True)

-- 8,19-19 8,24-27 


-- Problem: last u should be of type a where a is the argument type of x 
v66 = \x -> let f = \y -> y x                                 
            in f (\z -> z) (f (\u -> u))
      
-- 7,28-40


-- Problem: there are several ways to fix the program. No oracle given.                
v67 = \z -> let x = z                                         
            in (let y = z 1 in x True)
      
-- 7,27-27


-- Problem: t and y should be swapped
v68 = \f y (h:t) -> t y                                       

-- 6,21-23


-- Problem: ++ should be +                               
f5 0 n = []
f5 m n = m ++ n : (f5 (m-1) n)

-- Type safe in Haskell


-- Problem: there are several ways to fix the program. No oracle given.                
f6 x = if x > 3 then x else 1.1                        

-- Type safe in Haskell


-- Problem: ++ should be +
f4 [] = []                                              
f4 (0 : t) = f4 t
f4 (h : t) = h/2.0 ++ f4 t

-- Type safe in Haskell


-- Problem: body of the first alternative doesn't have right type
fail3 p [] = p + p                                      
fail3 p (h:t) = if p True then [h] else t

-- 7,32-34 


-- Problem: [0.0] should be 0.0
f 1 x = [0.0]                                          
f n x = n*x + f (n-1) x

-- Type safe in Haskell


-- Problem: == should of type Int -> Int -> Int
fac n = if n == 0 then 1 else n * fac (n == 1)          

-- 6,39-46


-- Problem: there are several ways to fix the program. No oracle given.                
v69 = \x -> if x then x +1 else x - 2.2                       

-- 6,16-16


-- Problem: there are several ways to fix the program. No oracle given.                
v691 = \x -> (x + 1 ) (if x then x + 1 else x - 2.2)           

-- 6,27-27


-- Problem: there are several ways to fix the program. No oracle given.                
f (c:cs) (i:is) = if i > 0 then f cs is                
                           else f is (c:[2.2])

-------------------
-- Helping students understand polymorphic type errors

-- Type safe in Haskell


-- Problem: there are several ways to fix the program. No oracle given.                
m f [] = []                                             
m f (x:xs) = f xs : m f x

-- 7,25-25


-- Problem: v should be of type Bool
f [] = False                                            
f [v] = v
f (h1:h2:t) = h1 `gt` h2
gt :: Int -> Int -> Bool
gt = undefined

-------------------
-- Unification source-tracking with application to diagnosis of type inference

-- 7,9-9


-- Problem: there are several ways to fix the program. No oracle given.                
f x = if x then x+1 else x                              

-- 6,10-10


-- Problem: there are several ways to fix the program. No oracle given.                
v73 = \ f g x -> if f ( g x) then let v = f x in g v
                             else let z = g x in z + 1

-------------------
-- explaining type errors in polymorphic languages

-- 7,50-54  6,45-45


-- Problem: the condition b should of type Bool
v74 = \a -> ( \b -> if b then b else a) True + 3              

-- 6,24-24


-- Problem: True in the second line should be of type Int
v75 = \a b -> ((if True then a else b, \c -> b c),            
              (b ((\x y -> y) 3 True), a 2 + 4 ))

-------------------
-- Debugging Type Errors

-- 7,33-36


-- Problem: there are several ways to fix the program. No oracle given.                
v80 = \f g a -> (f a, f 1, g a, g True)

--------------------
-- Typehope: There is hope for your type erros              

-- 6,25-25 6,35-38


-- Problem: the annotation should be [Char] -> Int
len :: [Int] -> Int                                   
len ls = len' 0 ls
    where len' n [] = n
          lengths n (_:xs) = len' (n+1) xs       
v81 = len "abc"          
-- 6,8-12
