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

(* $Id: constraintPrettyPrinter.mli 421 2006-12-22 09:27:42Z regisgia $ *)

(** The constraint over equality between terms. *)
type formula = 
    (MultiEquation.crterm, MultiEquation.variable) Constraint.type_constraint

(** Pretty printer for [formula]. 
    The connectors' representations can be redefined 
    (labels [forall, exists, andsym]). 
    A [user_name_from_int] function can be given to generate fresh strings 
    from integers.
    See {!PrettyPrinter.mode}. *)
val printf_constraint :
  ?forall:string ->
  ?exists:string ->
  ?andsym:string ->
  ?before:(formula -> 'a) ->
  ?after:(formula -> 'b) ->
  ?user_name_from_int:(int -> string) ->
  PrettyPrinter.mode ->
  formula ->
  unit
