datatype 'a  alist  = Nil 
                      | Cons of 'a * ('a alist)
                   
val x = (
let  fun map f l = 
    case l of
      Nil => Nil 
    | Cons (x,q) => Cons (f x, map q f)  (* should be map f q *)
in map (fn x => 1) Nil
end )
