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

(* ##########################################################
#####################################################################
############################### *)
let doubleIncrement (impl : 'a t1) =
  client1 {ctr1=impl.ctr1;
	   incr1=(fun c -> impl.incr1 c; impl.incr1 c);
	   allowDecr1=impl.allowDecr1;
	   decr1=impl.decr1;
	   print1=impl.print1}

let myClient1 (impl : 'a t1) =
  let x = ref 0 in
  impl.allowDecr1 x;
  impl.decr1 x;
  print_string "Got to the end\n"

let dynamicCheck (impl : 'a t1) =
  client1 {ctr1=impl.ctr1;
		   incr1=impl.incr1;
		   allowDecr1=(fun c -> c.decr1=impl.decr1);
		   decr1=raise ProtocolViolation;
		   print1=impl.print1}

let withholdDecr (impl : 'a t1) =
  client2 {ctr2=impl.ctr1;
		   incr2=impl.incr1;
		   allowDecr2=(fun c -> impl.decr1);
		   print2=impl.print1}

(*let useTypes (impl : 'a t1) =
  client3 {ctr3=impl.ctr1;
		   incr3=impl.incr1;
		   allowDecr3=(fun ctr -> impl.allowDecr1 ctr);
		   decr3=(fun ctr -> impl.allowDecr1 ctr) impl.ctr1;
		   print3=impl.print1}*)
(* 43,27-44 *)
