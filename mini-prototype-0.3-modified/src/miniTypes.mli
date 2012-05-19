(**************************************************************************)
(*  Mini, a type inference engine based on constraint solving.            *)
(*  Copyright (C) 2006. François Pottier, Yann Régis-Gianas               *)
(*  and Didier Rémy.                                                      *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; version 2 of the License.               *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  General Public License for more details.                              *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *)
(*  02110-1301 USA                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: miniTypes.mli 421 2006-12-22 09:27:42Z regisgia $ *)

open Positions
open MiniAst
open MiniTypingEnvironment
open MultiEquation
open CoreAlgebra
open Constraint

(** This module transforms types from the user's syntax to the
    internal representation of the inference engine. *)

(** [extract_type] examines an expression and looks for a sufficiently 
    explicit type annotation. 
    If it finds one, it returns the type annotation, 
    together with the expression (deprived of its annotation). 
    Otherwise, it raises [Not_found]. *)
val extract_type : expression -> typ * expression

(** [recursive_value_definition_kind] tests if a recursive definition
    is annotated or not. *)
type recursive_value_definition_kind =
    Implicit of name * expression
  | Explicit of name * typ * expression
  | NotPVar

(** [explicit_or_implicit p e] tests if a definition is annotated or 
    not and normalizes it such that type constraint can be found
    at the top of the term. For instance:
    \(x:int). (x : int) is normalized into (\x.x : int -> int). *)
val explicit_or_implicit :
  pattern -> expression -> recursive_value_definition_kind

(** [variables_of_typ ty] returns the type variables of [ty]. *)
val variables_of_typ : typ -> Misc.StringSet.t

(** [arrow env x1 x2] builds the internal representation of the 
    type [x1 -> x2]. *)
val arrow :
  environment -> variable arterm -> variable arterm -> variable arterm

(** [arity (t1 -> ... -> tn)] returns [n]. *)
val arity : typ -> int

(** [tycon t xs] builds the internal representation of the type [t xs]. *)
val tycon : environment -> tname -> variable arterm list -> variable arterm

(** [intern env ty] converts [ty] into its internal representation. *)
val intern : position -> environment -> typ -> crterm

(** [internal_let_env env fqs rqs] internalizes the flexible variables
    [fqs] and the rigid variables [rqs] into [env]. *)
val intern_let_env : position -> environment -> tname list -> tname list ->
  variable list * variable list * environment
    
(** [intern_scheme env x fqs ty] returns the internal representation
    of the type scheme [forall fqs.ty] and the binding of [x] to it. *)
val intern_scheme : position -> environment -> string -> tname list ->
  typ -> (crterm, variable) scheme
