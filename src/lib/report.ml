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

type 'a result = {
  value: 'a;
  good: string list;
  bad: ('a * string list) list
}

type t = config list

and config =
    | Netconfig of netconfig
    | Hardware of hardware
    | Service of service
 
and netconfig = {
  nc_name: string;
  nc_kind: Dtd.kind result;
  nc_devices: netdevice list;
}

and netdevice = {
  nd_name: string;
  nd_target: Dtd.area;
  nd_state: [ `Up | `Down ] result;
}

and hardware = {
  h_name: string;
  h_desc: hardware_desc list;
}

and hardware_desc =
    | Memory of memory
    | Disk of disk
    | Cpu of cpu

and memory = Dtd.memory result

and disk = Dtd.disk result

and cpu = Dtd.cpu result

and service = {
  s_name: string;
  s_nodes: node list;
}

and node = {
  n_role: string;
  n_type: string option;
  n_ha: Dtd.ha option;
  n_desc: node_desc list;
}

and node_desc =
    | Mount of mount
    | Daemon of daemon
    | Packages of packages
    | System of system
    | File of file

and mount = Dtd.mount result
and daemon = Dtd.daemon result
and file = Dtd.file result * Digest.t

and packages = {
  p_status: (string * [ `Installed | `Absent ]) list result;
  p_match: [ `Exact | `Subset ] result;
}

and system = {
  sys_name: string;
  sys_config: Dtd.sysconfig result list;
}
