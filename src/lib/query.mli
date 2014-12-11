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

val q_packages : Ast.Dtd.packages -> Ast.Result.packages
val q_daemon : Ast.Dtd.daemon -> Ast.Result.daemon
val q_mount : Ast.Dtd.mount -> Ast.Result.mount
val q_netconfig : Ast.Dtd.netconfig -> Ast.Result.netconfig
val q_memory : Ast.Dtd.memory -> Ast.Result.memory
val q_disk : Ast.Dtd.disk -> Ast.Result.disk
val q_cpu : Ast.Dtd.cpu -> Ast.Result.cpu
val q_system : Ast.Dtd.system -> Ast.Result.system
val q_file : Ast.Dtd.file -> Ast.Result.file

val cluster_to_result : Ast.Dtd.cluster -> Ast.Result.t
val merge_report_info :
  'a Ast.Report.info ->
  'a Ast.Report.info ->
  'a Ast.Report.info

val result_to_report : Ast.Result.t -> Ast.Report.t
val merge_reports : Ast.Report.t -> Ast.Report.t -> Ast.Report.t
val print_report : Ast.Report.t -> unit
