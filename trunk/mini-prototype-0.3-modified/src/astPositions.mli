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

(* $Id *)

(** This module provides shortcuts for AST position handling. *)

(**/**)
val position   : MiniAst.expression -> Positions.position
val joine      : MiniAst.expression -> MiniAst.expression -> Positions.position
val lposition  : MiniAst.expression list -> Positions.position
val tposition  : MiniAst.typ -> Positions.position
val tjoin      : MiniAst.typ -> MiniAst.typ -> Positions.position
val tlposition : MiniAst.typ list -> Positions.position
val bposition  : MiniAst.binding -> Positions.position
val bjoin      : MiniAst.binding -> MiniAst.binding -> Positions.position
val blposition : MiniAst.binding list -> Positions.position
val vposition  : 'a * 'b * 'c * 'd -> 'a
val vlposition :
  (Positions.position * 'a * 'b * 'c) list -> Positions.position
val tdposition : 'a * 'b * 'c * 'd -> 'a
val tdlposition:
  (Positions.position * 'a * 'b * 'c) list -> Positions.position
val pposition  : MiniAst.pattern -> Positions.position
val pjoin      : MiniAst.pattern -> MiniAst.pattern -> Positions.position
val plposition : MiniAst.pattern list -> Positions.position
