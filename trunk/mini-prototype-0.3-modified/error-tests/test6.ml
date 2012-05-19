(* Type constructor clash involving datatype declarations.
  *
  * The programming error is that we wrote 'c instead of 'b in
  * Green's definition.  The slicer accurately reports the parts
  * of the datatypes involved in the type errors.
  *)
 
type firsttype: * -> * -> * -> *      = forall a b c.
                           Red    : (a * b * c) -> firsttype a b c
                         | Blue   : (a * b * c) -> firsttype a b c
                         | Pink   : (a * b * c) -> firsttype a b c
                         | Green  : (a * b * c) -> firsttype a b b
                         | Yellow : (a * b * c) -> firsttype a b c
                         | Orange : (a * b * c) -> firsttype a b c

type secondtype: * -> * -> *   = forall a b.
                        SpeCol : ((firsttype a a b) * b) -> secondtype a b 
                      | UniCol : ((firsttype a a a) * b) -> secondtype a b

let  trans t =
     match t with 
     Red    (x, y, z) =>  Blue   (y, x, z)
   | Blue   (x, y, z) =>  Pink   (y, x, z)
   | Pink   (x, y, z) =>  Green  (y, x, z)
   | Green  (x, y, z) =>  Yellow (y, x, z)
   | Yellow (x, y, z) =>  Orange (y, x, z)
   | Orange (x, y, z) =>  Red    (y, x, z)
   end

let touni t =
     match t with 
     Red    (x, _, _) => Red    (x, x, x)
   | Blue   (x, _, _) => Blue   (x, x, x)
   | Pink   (x, _, _) => Pink   (x, x, x)
   | Green  (x, _, _) => Green  (x, x, x)
   | Yellow (x, _, _) => Yellow (x, x, x)
   | Orange (x, _, _) => Orange (x, x, x)
   end

let  getLast t =
     match t with 
     Red    (_, _, z) => z
   | Blue   (_, _, z) => z
   | Pink   (_, _, z) => z
   | Green  (_, _, z) => z
   | Yellow (_, _, z) => z
   | Orange (_, _, z) => z
   end

let  pretrans x =
     match x with
     SpeCol (col, b) =>
     (*if b = getLast col
     then*)  SpeCol (trans col, b)
     (* else UniCol (touni col, b)) *)
   | UniCol (_,_)  => x
   end

let x = SpeCol (Red (2, 2, 'a'), 'b')
let y = pretrans x
