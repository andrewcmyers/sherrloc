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

(* $Id: miniPrettyPrinter.mli 421 2006-12-22 09:27:42Z regisgia $ *)

(** This modules instanciates {!PrettyPrinter} for the Mini language. *)

(** [print_expression e] pretty-prints [e] according to the active 
    printing mode. *)
val print_expression    : MiniAst.expression -> unit

(** [print_binding b] pretty-prints [b] according to the active 
    printing mode. *)
val print_binding       : MiniAst.binding -> unit

(** [print_binding p] pretty-prints [p] according to the active 
    printing mode. *)
val print_program       : MiniAst.program -> unit

(** [print_type paren t] pretty-prints [t] according to the active 
    printing mode. If [paren] is set, all the parenthesis are shown. *)
val print_type		: ?paren:bool -> MiniAst.typ -> unit

(** Set the active mode. *)
val active_mode         : PrettyPrinter.mode -> unit

(**/**)
val register_tasks      : Processing.task_name -> unit
val print_program_task  : Processing.task_name
