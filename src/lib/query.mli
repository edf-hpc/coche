(****************************************************************************)
(*  Copyright (C) 2013-2014 EDF S.A.                                        *)
(*                                                                          *)
(*  This file is part of Coche.                                             *)
(*                                                                          *)
(*  Coche is free software: you can redistribute it and/or modify it        *)
(*  under the terms of the GNU General Public License as published by the   *)
(*  Free Software Foundation, either version 3 of the License, or (at your  *)
(*  option) any later version.                                              *)
(*                                                                          *)
(*  Coche is distributed in the hope that it will be useful, but WITHOUT    *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or   *)
(*  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License   *)
(*  for more details.                                                       *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with Coche.  If not, see <http://www.gnu.org/licenses/>.          *)
(*                                                                          *)
(****************************************************************************)

val q_netconfig :
  (Ast.classes -> bool) ->
  Ast.Dtd.netconfig ->
  Ast.Result.netconfig

val q_hardware :
  (Ast.classes -> bool) ->
  Ast.Dtd.hardware ->
  Ast.Result.hardware

val q_service :
  (Ast.classes -> bool) ->
  Ast.Dtd.service ->
  Ast.Result.service

val run :
  hostname:string ->
  Ast.Dtd.cluster ->
  Ast.Result.t
