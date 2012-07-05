type list : * -> * = Nil : forall a. list a 
                   | Cons : forall a. a * list a -> list a
                   
let x = (
  let rec map f l = 
    match l with  
      Nil => Nil 
    | Cons (x,q) => Cons (f x, map q f)  (* should be map f q *)
    end
  in map (\x. 1) Nil
)
