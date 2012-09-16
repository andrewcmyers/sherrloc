datatype 'a  alist  = Nil 
                      | Cons of 'a * ('a alist)
                   
val x = (
let  fun map l f = (* should be map f l *)
    case l of
      Nil => Nil 
    | Cons (x,q) => Cons (f x, map f q)  
in map (fn x => 1) Nil
end )
