(* #############################################################
############################################################
*)

open Event

let sendNow ch a = sync (send ch a)
let recvNow ch = sync (receive ch)

type acct = float channel * float channel * float channel * float channel

let mkAcct () =
  let
    inGetCh  = new_channel() and
    inPutCh  = new_channel() and
    outGetCh = new_channel() and
    outPutCh = new_channel() and
    bal = ref 0.0
  in
    let rec loop () =
      sync(choose[
          wrap (receive inGetCh) (fun f -> bal := !bal -. f; sendNow outGetCh !bal);
          wrap (receive inPutCh) (fun f -> bal := !bal +. f; sendNow outPutCh !bal)]);
      loop ()
    in Thread.create loop (), (inGetCh,inPutCh,outGetCh,outPutCh)

let get acct f =
  let (inGetCh,_,outGetCh,_) = acct in
  sendNow inGetCh f; recvNow outGetCh

let put acct f =
  let (_,inPutCh,_,outPutCh) = acct in
  sendNow inPutCh f; recvNow outPutCh


let test () =
  let acct = mkAcct () in (
    print_newline (put acct 10)
  )
(*
############################
############################
############################
*)

test ()
(* (thread,acct)=mkAcct() *)
(* 37,6-22 *)
