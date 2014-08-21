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

val q_packages : Dtd.packages -> Result.packages
val q_daemon : Dtd.daemon -> Result.daemon
val q_mount : Dtd.mount -> Result.mount
val q_netconfig : Dtd.netconfig -> Result.netconfig
val q_memory : Dtd.memory -> Result.memory
val q_disk : Dtd.disk -> Result.disk
val q_cpu : Dtd.cpu -> Result.cpu
val q_system : Dtd.system -> Result.system
val q_file : Dtd.file -> Result.file

val cluster_to_result : Dtd.cluster -> Result.t
val merge_results:
  'a Report.result ->
  'a Report.result ->
  'a Report.result

val result_to_report : Result.t -> Report.t
val merge_reports : Report.t -> Report.t -> Report.t
val print_report : Report.t -> unit
