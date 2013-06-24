(* #############################################################
############################################################
*)

(* ################################################## *)
open Thread
open Event

exception Unimplemented

type acct = {
  bal : float ref;
  avail : float ref;
  ch1 : float channel;
  ch2 : float channel;
  ch3 : float channel;
  ch4 : float channel;
  }

let mkAcct () = 
  {bal=0.0;avail=0.0;
   ch1=new_channel();
   ch2=new_channel();
   ch3=new_channel();
   ch4=new_channel()
  }

let get acct f = 
  if(!(acct.avail) > f)
  then (acct.bal := !(acct.bal) -. f
        a.avail := !(a.avail) -. f);
  raise Unimplemented

let put acct f = 
  acct.bal := !(acct.bal) +. f;
  acct.avail := !(a.avail) +.(if f<500. then f else 500.); 
  raise Unimplemented
  
(* 21,7-10  21,17-20 *)
