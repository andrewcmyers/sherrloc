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

(* $Id: processing.mli 421 2006-12-22 09:27:42Z regisgia $ *)

(** A simple task manager.
    It enables the registration of the options and the control
    of what has to be done by the program. 
*)

(** {2 Tasks} *)

type task_name = string
type process_type = task_name 
type process_types = process_type list list
type process_data  
type process_datas 

type process = 
    { 
      input_type : process_type list list;
      output_type: process_type;
      code       : process_types * process_datas -> process_type * process_data
    }

type task

type options = (Arg.key * Arg.spec * Arg.doc) list * Arg.anon_fun

val register: 
  task_name -> options -> task_name list list 
  -> ('a -> 'b) -> (unit -> bool) -> unit

val is_task_traced: task_name -> bool

val get_registered_tasks : unit -> task_name list

(** {2 Processing} *)

val execute: 
  default_start:task_name -> default_end:task_name -> usage:string -> unit

val debug: string -> unit

