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


(* ###################### *)
let dynamicCheck (impl : 'a t1) =
  let allowDecr : int ref = ref 0 in
  client1 {ctr1=impl.ctr1;
	   incr1=(fun c -> impl.incr1 c);
	   allowDecr1=(allowDecr:=1;impl.allowDecr1);
	   decr1=(if (!allowDecr) then impl.decr1 else raise ProtocolViolation);
	   print1=impl.print1}

(* ######################

##
##################################
#############################
###########
##########
##

############################
#################################
#####################

############################
#############################
#####################
 *)

(* the user changed the type of allowDecr to bool ref later *)
(* 37,19-21  37,33-34  40,27-28  *)
