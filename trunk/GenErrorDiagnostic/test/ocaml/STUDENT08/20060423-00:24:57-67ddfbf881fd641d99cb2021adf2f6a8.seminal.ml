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
######################################################################
############################### *)
let doubleIncrement (impl : 'a t1) =
  client1 {ctr1=impl.ctr1;
	   incr1=(fun c -> impl.incr1 c; impl.incr1 c);
	   allowDecr1=impl.allowDecr1;
	   decr1=impl.decr1;
	   print1=impl.print1}

(* ################### *)
let dynamicCheck (impl : 'a t1) =
  let isDecrAllowed = !false in
    client1 {
      ctr1= impl.ctr1;
      incr1 = impl.incr1;
      allowDecr1 = (isDecrAllowed := true; impl.allowDecr1);
      decr1 = if !isDecrAllowed then impl.decr1 else raise ProtocolViolation;
      print1 =  impl.print1;
    }

(* ################### *)
let withholdDecr (impl : 'a t1) =
  raise Unimplemented

(* ################### *)
let useTypes (impl : 'a t1) =
  raise Unimplemented
(* 36,23-28 *)
