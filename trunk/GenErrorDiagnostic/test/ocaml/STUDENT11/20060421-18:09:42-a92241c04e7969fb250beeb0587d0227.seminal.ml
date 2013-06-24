open Hw4clients

exception Unimplemented
exception ProtocolViolation

(* ############################################################### *)

let ctr = ref 0

let incr ctr = ctr := (!ctr) + 1

let allowDecr ctr = () (* ################################################ *)

let decr ctr = ctr := (!ctr) - 1

let print ctr = print_string (string_of_int !ctr)

let impl : (int ref) t1 = { ctr1=ctr;
			    incr1=incr;
			    allowDecr1=allowDecr;
			    decr1=decr;
			    print1=print }

let client1 (impl : 'a t1) =
  impl.incr1 impl.ctr1;
  impl.print1 impl.ctr1;
  impl.allowDecr1 impl.ctr1;
  impl.decr1 impl.ctr1;
  impl.ctr1 

let client2 (impl : 'a t2) =
  impl.incr2 impl.ctr2;
  impl.print2 impl.ctr2;
  let decr = impl.allowDecr2 impl.ctr2 in
  decr impl.ctr2;
  impl.ctr2 

(* ##########################################################
######################################################################
############################### *)
let doubleIncrement (impl : 'a t1) = 
  client1 {ctr1=impl.ctr1;
	   incr1=(fun c -> impl.incr1 c; impl.incr1 c);
	   allowDecr1=impl.allowDecr1;
	   decr1=impl.decr1;
           print1=impl.print1;
          }


(* ###################### *)
let dynamicCheck (impl : 'a t1) =
  let allowDecr : bool ref = ref false in
  !(client1 {ctr1=impl.ctr1;
	   incr1=(fun c -> impl.incr1 c);
	   allowDecr1=(fun c -> 
                       allowDecr:=true;
                       impl.allowDecr1 c);
	   decr1=(fun c -> 
                       if (!allowDecr) 
                       then (impl.decr1 c) 
                       else raise ProtocolViolation);
	   print1=impl.print1})


(* ###################### *)
let withholdDecr (impl : 'a t1) =
  !(client2 {ctr2=impl.ctr1;
           incr2=impl.incr1;
	   allowDecr2 = (fun c -> impl.allowDecr1 c; impl.decr1);
           print2=impl.print1})


(* ###################### *)
let useTypes (impl : 'a t1) =
  raise Unimplemented


let _ = impl.print1 (doubleIncrement impl); print_endline ""
let _ = print (string_of_int(dynamicCheck impl)); print_endline ""
let _ = print (string_of_int(withholdDecr impl)); print_endline ""

(* should use print_string *)
(* 79,8-13 *)
