
exception Unimplemented
exception RuntimeTypeError
exception DoesNotTypecheck of string

(****** Syntax for our language, including types (do not change) *****)
type exp = Var of string 
         | Lam of string * typ * exp 
	 | Apply of exp * exp
	 | Closure of string * exp * (env ref)
	 | Int of int
	 | Plus of exp * exp
	 | If of exp * exp * exp
	 | RecordE of (string * exp) list
	 | RecordV of (string * (exp ref)) list (* ################## *)
	 | Get of exp * string
	 | Set of exp * string * exp
	 | Letrec of typ * string *  string * typ * exp
	 | Cast of exp * typ

and env = (string * exp) list

and access = Read | Write | Both

and typ = IntT
        | ArrowT of typ * typ
	| RecordT of (string * typ * access) list

and ctxt = (string * typ) list

(****** Interpreter for our language (do not change) *****)
let rec interp env e =
  match e with
    Var s -> (try List.assoc s env with Not_found -> raise RuntimeTypeError)
  | Lam(s,_,e2) -> Closure(s,e2,ref env) 
  | Closure _ -> e 
  | Apply(e1,e2) ->
      let v1 = interp env e1 in
      let v2 = interp env e2 in
      (match v1 with
	Closure(s,e3,env2) -> interp((s,v2)::(!env2)) e3
      | _ -> raise RuntimeTypeError)
  | Plus(e1,e2) ->
      let v1 = interp env e1 in
      let v2 = interp env e2 in
      (match(v1,v2) with
	(Int i, Int j) -> Int (i+j)
      | _ -> raise RuntimeTypeError)
  | If(e1,e2,e3) ->
      let v1 = interp env e1 in
      (match v1 with
	Int 0 -> interp env e3
      | Int _ -> interp env e2
      | _ -> raise RuntimeTypeError)
  | Int _ -> e
  | RecordV _ -> e
  | RecordE lst -> RecordV (List.map (fun (s,r) -> (s,ref(interp env r))) lst)
  | Get(e,s) -> 
      (match interp env e with
	RecordV lst -> 
	  (try !(List.assoc s lst) with Not_found -> raise RuntimeTypeError)
      | _ -> raise RuntimeTypeError)
  | Set(e1,s,e2) ->
      (match interp env e1 with	
	RecordV lst -> 
	  let r=try List.assoc s lst with Not_found -> raise RuntimeTypeError in
	  let ans = interp env e2 in 
	  r := ans; ans
      | _ -> raise RuntimeTypeError)
  | Letrec(_,f,x,_,e) ->
      let r = ref env in
      let c = Closure(x,e,r) in
      let _ = r := (f,c)::(!r) in
      c
  | Cast(e,t) -> interp env e

let interp e = interp [] e

(***** helper functions provided to you (do not change) *****)
let fields_unique lst = (*raise exception if same field name appears > 1 time*)
  let rec loop lst1 lst2 =
    match lst2 with
      [] -> ()
    | (s,t,a)::tl -> 
	if (List.exists (fun (s2,_,_) -> s2=s) lst1)
	then raise (DoesNotTypecheck "")
	else loop ((s,t,a)::lst1) tl in
  loop [] lst

let rec checkType t = (* ################################################### *)
  match t with
    IntT -> ()
  | ArrowT(t1,t2) -> checkType t1; checkType t2
  | RecordT lst -> fields_unique lst; List.iter (fun (_,t,_) -> checkType t) lst

let rec getFieldType lst str = (* ############################################################### *)
  match lst with
    [] -> None
  | (s,t,a)::tl -> if s=str then Some (t,a) else getFieldType tl str

(******** Problem 1: complete subtype and typecheck *********)
let rec subtype t1 t2 = (* ############################# *)
  match t1,t2 with
    (IntT, IntT) -> true
  | (IntT, ArrowT(arrow1, arrow2)) -> subtype t1 arrow2
  | (ArrowT(arrow1, arrow2), IntT) -> subtype arrow2 t2
  | (ArrowT(arrow11, arrow12), ArrowT(arrow21, arrow22))-> 
      ((subtype arrow21 arrow11) & (subtype arrow12 arrow22))
  | (IntT, RecordT lst) -> false
  | (RecordT lst, IntT) -> false
  | (ArrowT(arrow1, arrow2), RecordT lst)->
      subtype arrow2 t2
  | (RecordT lst, ArrowT(arrow1, arrow2))->
      subtype t1 arrow2
  | (RecordT lst1, RecordT lst2) ->
      let func = 
	(fun typCheck ->
	  let (s,t,a) = typCheck in
	  let inRec = getFieldType lst2 s in
	  match inRec with 
	    None -> false
	  | Some(typ, access) -> 
	      if (typ = t & access = a) then true else false) in
      List.for_all func lst1

let rec typecheck ctxt e = (* ########################### *)
  (* ###################### *)
  match e with
    Var s -> (try List.assoc s ctxt with Not_found -> raise (DoesNotTypecheck ""))
  | Lam(s,t,e) -> checkType t; ArrowT(t,typecheck ((s,t)::ctxt) e)
  | Closure _ -> raise (DoesNotTypecheck "not a source program")
  | Int _ -> IntT
  | Plus(e1,e2) ->
      if subtype (typecheck ctxt e1) IntT && subtype (typecheck ctxt e2) IntT
      then IntT
      else raise (DoesNotTypecheck "")
  | If (e1,e2,e3) -> 
      let e1Check = subtype (typecheck ctxt e1) IntT in
      let e2Check = subtype (typecheck ctxt e2) (typecheck ctxt e3) in
      let e3Check = subtype (typecheck ctxt e3) (typecheck ctxt e2) in
      if (e1Check & ((e2Check || e3Check))) then
	(if (e2Check) then (checkType (typecheck ctxt e2);(typecheck ctxt e2))
	else (checkType (typecheck ctxt e3); (typecheck ctxt e3)))
	  else raise (DoesNotTypecheck "")
  | RecordE(lst)-> 
      let retRec = 
	RecordT(
	List.map (fun (s, e) -> 
	(s, (typecheck ctxt e),Both)) lst) in
      checkType retRec; retRec
  | RecordV(lst) -> raise (DoesNotTypecheck "")
  | Get (e,s) ->
      (match e with 
	RecordV lst -> 
	  let fieldType = getFieldType lst str in
	  (match fieldType with 
	    None -> raise (DoesNotTypecheck "")
	  | Some(typ, access) -> 
	      (match access with 
		Write -> raise (DoesNotTypecheck "")
	      | _ -> typ))
      | _ -> raise (DoesNotTypecheck ""))
  | Set (e, s, _) ->
      (match e with 
	RecordV lst -> 
	  let fieldType = getFieldType lst str in
	  (match fieldType with 
	    None -> raise (DoesNotTypecheck "")
	  | Some(typ, access) -> 
	      (match access with 
		Read -> raise (DoesNotTypecheck "")
	      | _ -> typ))
      | _ -> raise (DoesNotTypecheck ""))
  | Letrec(retType, _, _, argType, _) ->
      checkType argType; checkType retType;ArrowT(argType, retType)
  | Cast(e,t) -> checkType t; 
      if (subtype (typecheck ctxt e) t) then t else raise (DoesNotTypecheck "")

let typecheck e = typecheck [] e

(********** examples and testing ***********)

(* #########################################################################
########################################################### *)

(* ################################################################ *)
let lam x t e = Lam(x,t,e)
let app e1 e2 = Apply(e1,e2)
let vx = Var "x"
let vy = Var "y"
let vf = Var "f"

(* #################################################### *)
(* #############################################################################
##########
############################################################
#################################################
###########################################################

######################
###################

#############################################################################
##########
#########################################################################
#######################
##################
##############################
######################################################
##########################################################
###########################
######################################
##########################################

######################
###################

###################
#####################################################
#######################################################
#####################################################
#######################################################

 *)

(* 155,32-35  166,32-35 *)
