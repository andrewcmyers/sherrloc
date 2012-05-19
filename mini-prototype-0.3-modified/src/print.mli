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

(* $Id: print.mli 421 2006-12-22 09:27:42Z regisgia $ *)

(** This module provides a simple pretty-printer for the terms
    maintained by a unifier. *)

(** [reset()] clears [print]'s memoization table. *)
val reset: unit -> unit

(** [print context x] returns a printable representation of the
    object [x]. Consecutive calls to [print] share the same
    variable naming conventions, unless [reset] is called in
    between. The context [context] may be used to pass additional
    information to the printing function. *)
val print_variable: 
  ?user_name_from_int:(int -> string) -> bool -> MultiEquation.variable -> string

val print_term: 
  ?user_name_from_int:(int -> string) -> bool -> MultiEquation.crterm -> string

