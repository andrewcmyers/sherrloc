(* #############################################################
############################################################
*)

(* ################################################## *)
open Thread
open Event

exception Unimplemented

type acct_in = {
  bal : float ref;
  avail : float ref
  }

type acct = {
  put_in : float channel;
  put_out : float channel;
  get_in : float channel;
  get_out : float channel
  }

let mkAcct () = 
  let putin = new_channel() in
  let putout = new_channel() in
  let getin = new_channel() in
  let getout = new_channel() in
  let acct_in = { bal=ref 0.0; avail=ref 0.0 } in
  let rec get f = 
    (if(!(acct_in.avail) > f)
     then (acct_in.bal := !(acct_in.bal) -. f;
         acct_in.avail := !(acct_in.avail) -. f);
     sync(send !(acct_in.bal) getout))
  in
  let rec put f = 
    (acct_in.bal := !(acct_in.bal) +. f;
     acct_in.avail := !(acct_in.avail) +. (if f<500. then f else 500.);
     sync(send !(acct_in.bal) putout))
  in
  let rec loop () = 
    let _ = sync(choose[wrap (receive getin) (get);
                        wrap (receive putin) (put)])
    in
    loop ()
  in  
  let _ = Thread.create loop () in
  {put_in =putin;
   put_out=putout;
   get_in =getin;
   get_out=getout
  }

let get acct f = 
  raise Unimplemented

let put acct f = 
  raise Unimplemented
  

(* 33,15-29  38,15-29 *)
