type list : * -> * = Nil : forall a. list a 
                   | Cons : forall a. a * list a -> list a
                   
let x = (
  let rec map l f = 
    match l with  
      Nil => Nil 
    | Cons (x,q) => Cons (f x, map q f)  
    end
  in map (\x. 1) Nil (* should be map Nil (\x. 1) *)
)
