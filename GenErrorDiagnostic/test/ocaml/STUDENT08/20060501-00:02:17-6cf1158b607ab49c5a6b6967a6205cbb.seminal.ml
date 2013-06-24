open Thread
open Event

let sendNow ch a = sync (send ch a)
let recvNow ch = sync (receive ch)

type action = Put of float | Get of float

type acct = action channel * float channel
let mkAcct () =
  let inCh  = new_channel() in
  let outCh = new_channel() in
  let bal   = ref 0.0       in (* ##### *)
  let rec loop () =
    (match recvNow inCh with (* ###### *)
      Put f -> bal := !bal +. f; 
    | Get f -> bal := !bal -. f);(*allows overdraw*)
    sendNow outCh !bal; loop ()
  in Thread.create (); (inCh,outCh)

let get acct f =
  let inCh,outCh  = acct in
  sendNow inCh (Get f); recvNow outCh

let put acct f =
  let inCh,outCh  = acct in
  sendNow inCh (Put f); recvNow outCh

(* 19,19-21 *)
