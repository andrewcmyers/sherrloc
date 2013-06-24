(* #############################################################
############################################################
*)

(* ################################################## *)
open Thread
open Event

exception Unimplemented

type acct = float channel * float channel * float channel * float channel

let mkAcct () = 
  let putToAcct = new_channel() in
  let putFromAcct = new_channel() in
  let getToAcct = new_channel() in
  let getFromAcct = new_channel() in
  let bal = ref 0.0 in
  let evs = [(wrap (receive putToAcct) (fun amt -> (amt, putFromAcct)));
             (wrap (receive getToAcct) (fun amt -> (0.0 -. amt, getFromAcct)))] in
  let rec loop () = 
    let (amt, fromAcct) =  sync (choose evs) in
    bal := !bal +. amt; sync (send fromAcct !bal); loop () in
  Thread.create loop (); (putToAcct, putFromAcct, getToAcct, getFromAcct)

let get acct f = 
  let (_,_,getToAcct,getFromAcct) = acct in
  sync (send getToAcct f); sync (receive getFromAcct)

let put acct f = 
  let (_,_,putToAcct,putFromAcct) = acct in
  sync (send putToAcct f); sync (receive putFromAcct)

let print_float = print_string string_of_float

let myAcct = mkAcct

let curBal = get myAcct 0.0
let _ = print_float curBal

let curBal = get myAcct 1.0
let _ = print_float curBal

let curBal = put myAcct 5.0
let _ = print_float curBal
(* 34,31-46 *)
