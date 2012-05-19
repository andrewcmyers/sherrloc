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

(* $Id: miniInfer.mli 421 2006-12-22 09:27:42Z regisgia $ *)

(** This module expresses the problem of type inference for MiniML 
    programs to the problem of constraint solving by a transformation
    of program into typing constraints. *)

open MiniAst
open MiniAlgebra
open Constraint
open Positions
open MiniTypingEnvironment

 (** Constraint contexts. *)
type context = tconstraint -> tconstraint

(** [infer_program p] generates a constraint context that describes 
    program [p]. *)
val infer_program: environment -> program -> environment * context

(** [generate_constraint p] generates a closed constraint that describes
    the typing of [p]. *)
val generate_constraint: program -> tconstraint
    
(** [init_env ()] returns a constraint context that defines the builtins
    of the source language. *)
val init_env: unit -> context * environment 

(** [remove_init_context] returns the context part that concerns the
    initial environment. *)
val remove_init_context: tconstraint -> tconstraint

(**/**)
val generate_constraint_task: Processing.task_name
val register_tasks: Processing.task_name -> unit
